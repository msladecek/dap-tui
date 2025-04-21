(local copas (require "copas"))
(local inspect (require "inspect"))
(local seq (require "pl.seq"))
(local socket (require "socket"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))
(local {: make-tui
        : usable-termsize}
  (require :dap-tui.terminal))
(local {: write-message
        : make-request
        : read-message}
  (require :dap-tui.message))

(fn load-source [path]
  (icollect [line (io.lines path)]
    line))

(fn make-handler [tui send-request]
  (var seq 1)
  (fn seq-next []
    (let [seq-val seq]
      (set seq (+ 1 seq-val))
      seq-val))

  (fn request [...]
    (send-request (make-request (seq-next) ...)))

  (local handler {})

  (var last-thread-id nil)

  (fn handler.handle-message [message]
    (tui.handle-command
      :add-event
      {:label (.. message.content.type " " (or message.content.event message.content.command ""))
       :content message})

    (case [message.content.type (or message.content.command message.content.event)]
      [:response :initialize]
      (do
        (request :attach {:connect {}})
        (request :setExceptionBreakpoints {:filters ["uncaught"]})
        (request :setFunctionBreakpoints {:breakpoints [{:name "do_something"}]})
        (request :configurationDone {}))

      [:event :stopped]
      (do
        (set last-thread-id message.content.body.threadId)
        (request :stackTrace {:threadId message.content.body.threadId}))

      [:event :terminated]
      nil ;; TODO

      [:response :stackTrace]
      (let [sources {}]
        (each [_ frame (ipairs message.content.body.stackFrames)]
          (request :scopes {:frameId frame.id})

          (let [path frame.source.path]
            (set (. sources path) (load-source path))))

        (tui.handle-command
          :add-event
          {:label "sources loaded"
           :type :sources-loaded
           :content sources}))

      [:response :scopes]
      (each [_ scope (ipairs message.content.body.scopes)]
        (request :variables {:variablesReference scope.variablesReference}))))

  (fn handler.handle-command [command]
    (case command
      :run
      (request :initialize {:clientName "dap-tui" :adapterID "debugpy"})

      :continue
      (request :continue {})

      :next
      (request :next {:threadId last-thread-id})
      ))

  handler)

(fn main []
  (local tui (make-tui))
  (tui.initialize)

  (local sock (copas.wrap (assert (socket.tcp))))
  (sock:connect "localhost" 5678)
  (sock:settimeout 1)

  (fn send-request [request]
    (tui.handle-command
      :add-event
      {:label (.. "request" " " (or request.content-data.command ""))
       :content {:headers request.headers
                 :content request.content-data
                 :content-raw request.content}})
    (write-message sock request))

  (local handler (make-handler tui send-request))
  (var should-run? true)

  (copas.addthread
    (fn []
      (while should-run?
        (let [message (read-message sock)]
          (when (and message (?. message :content :type))
            (handler.handle-message message))))))

  (copas.timer.new
    {:delay 0.2
     :recurring true
     :callback (fn [timer]
                 (if (not should-run?)
                   (timer:cancel)
                   (tui.handle-command :set-screensize (usable-termsize))))})

  (copas.addthread
    (fn []
      (while should-run?
        (local (char typ sequence) (t.input.readansi math.huge copas.pause))
        (case char
          :1 (tui.handle-command :select-window {:window-key :1})
          :2 (tui.handle-command :select-window {:window-key :2})

          :E (tui.handle-command :select-screen {:screen-id :events})
          :D (tui.handle-command :select-screen {:screen-id :debug})

          :h (tui.handle-command :move-cursor {:direction :left})
          :j (tui.handle-command :move-cursor {:direction :down})
          :k (tui.handle-command :move-cursor {:direction :up})
          :l (tui.handle-command :move-cursor {:direction :right})


          :r (handler.handle-command :run)
          :c (handler.handle-command :continue)
          :n (handler.handle-command :next)
          :s (case tui.active-screen
               :debug (handler.handle-command :step-in)
               :events (tui.handle-command :toggle-slow-write))
          :S (handler.handle-command :step-out)

          :q (set should-run? false)

          _ (tui.handle-command
              :add-event
              {:label (.. "unhandled key: " char)
               :content {:char char
                         :type typ
                         :sequence sequence}})))))

  (while should-run?
    (set copas.running true)
    (copas.step))
  (set copas.running false)

  (tui.shutdown))

(main)
