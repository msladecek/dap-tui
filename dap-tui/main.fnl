(local socket (require "socket"))
(local cjson (require "cjson.safe"))
(local inspect (require "inspect"))
(local seq (require "pl.seq"))
(local tablex (require "pl.tablex"))
(local copas (require "copas"))
(local {: make-tui} (require :dap-tui.terminal))

(fn parse-header [header-line]
  (let [loc (string.find header-line ": ")]
    (when loc
      (let [header-name (string.sub header-line 0 (- loc 1))
            header-value (string.sub header-line (+ loc 2))
            header-value-parsed (if (= "Content-Length" header-name)
                                  (tonumber header-value)
                                  header-value)]
        (values header-name header-value-parsed)))))

(fn make-empty-message []
  {:headers {}
   :content nil
   :content-raw nil})

(fn write-message [socket message]
  (each [header val (pairs message.headers)]
    (socket:send header)
    (socket:send ": ")
    (socket:send (tostring val))
    (socket:send "\r\n"))
  (socket:send "\r\n")
  (when message.content
    (socket:send message.content)))

(fn read-message [socket]
  (var message (make-empty-message))
  (var continue-reading? true)
  (while continue-reading?
    (local header-line (socket:receive))
    (if (and header-line (not= "" header-line))
      (let [(header val) (parse-header header-line)]
        (when header
          (tset message.headers header val)))
      (set continue-reading? false)))

  (when (. message.headers "Content-Length")
    (let [content (socket:receive (. message.headers "Content-Length"))]
      (set message.content-raw content)
      (set message.content (cjson.decode content))
      message)))

(fn make-request [seq command arguments]
  (let [content-data {:seq seq
                      :type :request
                      :command command
                      :arguments arguments}
        content-json (cjson.encode content-data)]
    {:headers {"Content-Length" (length content-json)}
     :content content-json}))

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
    (let [content (cjson.decode request.content)]
      (tui.handle-command
        :add-event
        {:label (.. "request" " " (or content.command ""))
         :content {:headers request.headers
                   :content content
                   :content-raw request.content}}))
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
