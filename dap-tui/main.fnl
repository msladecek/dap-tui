(local copas (require "copas"))
(local inspect (require "inspect"))
(local seq (require "pl.seq"))
(local socket (require "socket"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))
(local {:make-tui make-tui} (require :dap-tui.terminal))
(local {:write-message write-message
        :make-request make-request
        :read-message read-message}
  (require :dap-tui.message))

(fn make-handler [tui send-request]
  (var seq 1)
  (fn seq-next []
    (let [seq-val seq]
      (set seq (+ 1 seq-val))
      seq-val))

  (fn request [...]
    (send-request (make-request (seq-next) ...)))

  (local handler {})

  (fn handler.handle-message [message]
    (tui.handle-command
      :add-event
      {:label (.. message.content.type " " (or message.content.event message.content.command ""))
       :content message})

    (match [message.content.type (or message.content.command message.content.event)]
      [:response :initialize]
      (do
        (request :attach {:connect {}})
        (request :setExceptionBreakpoints {:filters ["uncaught"]})
        (request :configurationDone {}))

      [:event :stopped]
      (request :stackTrace {:threadId message.content.body.threadId})

      [:response :stackTrace]
      (each [_ frame (ipairs message.content.body.stackFrames)]
        (request :scopes {:frameId frame.id}))

      [:response :scopes]
      (each [_ scope (ipairs message.content.body.scopes)]
        (request :variables {:variablesReference scope.variablesReference}))))

  (fn handler.handle-command [command]
    (match command
      :run
      (request :initialize {:clientName "dap-tui" :adapterID "debugpy"})

      :continue
      (request :continue {})))

  handler)

(fn main []
  (local tui (make-tui))
  (tui.redraw)

  (local sock (copas.wrap (assert (socket.tcp))))
  (sock:connect "localhost" 5678)
  (sock:settimeout 0)

  (fn send-request [request]
    (tui.handle-command
      :add-event
      {:label (.. "request" " " (or request.content-data.command ""))
       :content {:headers request.headers
                 :content request.content-data
                 :content-raw request.content-json}})
    (write-message sock request))

  (local handler (make-handler tui send-request))
  (var should-run? true)

  (copas.addthread
    (fn []
      (while should-run?
        (let [message (read-message sock)]
          (when message
            (handler.handle-message message))))))

  (copas.timer.new
    {:delay 0.2
     :recurring true
     :callback (fn [timer]
                 (if
                   (not should-run?) (timer:cancel)
                   (tui.should-resize?) (tui.redraw)))})

  (copas.addthread
    (fn []
      (while should-run?
        (local (char typ sequence) (t.input.readansi math.huge copas.pause))
        (match char
          :1 (tui.handle-command :select-window {:window-key :1})
          :2 (tui.handle-command :select-window {:window-key :2})

          :E (tui.handle-command :select-screen {:screen-id :events})
          :D (tui.handle-command :select-screen {:screen-id :debug})

          :r (handler.handle-command :run)
          :c (handler.handle-command :continue)

          :R (tui.draw)

          :h (tui.handle-command :move-cursor {:direction :left})
          :j (tui.handle-command :move-cursor {:direction :down})
          :k (tui.handle-command :move-cursor {:direction :up})
          :l (tui.handle-command :move-cursor {:direction :right})

          :q (set should-run? false)

          _ (tui.handle-command
              :add-event
              {:label "unhandled key"
               :content {:char char
                         :type typ
                         :sequence sequence}})))))

  (while should-run?
    (set copas.running true)
    (copas.step))
  (set copas.running false)

  (tui.shutdown))

(main)
