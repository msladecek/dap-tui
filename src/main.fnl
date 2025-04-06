(local socket (require "socket"))
(local cjson (require "cjson.safe"))
(local inspect (require "inspect"))
(local t (require "terminal"))
(local sys (require "system"))
(local stringx (require "pl.stringx"))
(local seq (require "pl.seq"))
(local copas (require "copas"))


(var my-box-fmt (t.draw.box_fmt.copy t.draw.box_fmt.single))
(set my-box-fmt.post " ")
(set my-box-fmt.pre " ")

(var my-focused-box-fmt (t.draw.box_fmt.copy t.draw.box_fmt.double))
(set my-focused-box-fmt.post " ")
(set my-focused-box-fmt.pre " ")

(fn new-window [title pos-r pos-c height width]
  {:pos-r pos-r
   :pos-c pos-c
   :width width
   :height height
   :title title
   :start-r (+ 1 pos-r)
   :start-c (+ 1 pos-c)
   :content-width (- width 2)
   :content-height (- height 2)
   :content []
   :focused false
   :scroll-offset 0})

(fn new-list-window [...]
  (local window (new-window ...))
  (set window.cursor nil)
  window)

(fn list-window-mark-current-item [window prefix]
  (when window.cursor
    (t.cursor.position.stack.push (- (+ window.start-r window.cursor) 1) window.start-c)
    (t.output.write (.. prefix (. (. window.content window.cursor) :title)))
    (t.cursor.position.stack.pop)))

(fn list-window-add-item [window item]
  (list-window-mark-current-item window "  ")

  (set window.cursor (if window.cursor (+ 1 window.cursor) 1))
  (table.insert window.content item)

  (list-window-mark-current-item window "> "))

(fn list-window-active-item [window]
  (when window.cursor
    (. window.content window.cursor)))

(fn list-window-select-previous [window]
  (when (and window.cursor
             (< 1 window.cursor))
    (list-window-mark-current-item window "  ")
    (set window.cursor (- window.cursor 1))
    (list-window-mark-current-item window "> ")))

(fn list-window-select-next [window]
  (when (and window.cursor
             (< window.cursor (length window.content)))
    (list-window-mark-current-item window "  ")
    (set window.cursor (+ window.cursor 1))
    (list-window-mark-current-item window "> ")))

(fn window-add-content-line [window line]
  (let [line-no (length window.content)
        line-chunk (string.sub line 0 window.content-width)]
    (table.insert window.content line)
    (t.cursor.position.stack.push (+ window.start-r line-no) window.start-c)
    (t.output.write line-chunk))

  (t.output.flush)
  (t.cursor.position.stack.pop))

(fn window-replace-content [window new-content]
  (local new-content
    (if new-content
      (stringx.splitlines new-content)
      window.content))

  (local output [])
  (each [line-no line (-> (seq.iter new-content)
                          (seq.skip window.scroll-offset)
                          (seq.take window.content-height)
                          (seq.copy)
                          (ipairs))]
    (table.insert output (t.cursor.position.set_seq (- (+ window.start-r line-no) 1) window.start-c))
    (let [line-chunk (.. (string.sub line 0 window.content-width)
                         (string.rep " " (- window.content-width (length line))))]
      (table.insert output line-chunk)))

  (for [line-no (+ 1 (length new-content)) (math.min window.content-height (length window.content))]
    (table.insert output (t.cursor.position.set_seq (- (+ window.start-r line-no) 1) window.start-c))
    (table.insert output (string.rep " " window.content-width)))

  (set window.content new-content)
  (t.output.write (stringx.join "" output))
  (t.output.flush))

(fn window-content-scroll-down [window]
  (when (< window.scroll-offset (- (length window.content) window.content-height))
    (set window.scroll-offset (+ window.scroll-offset 1))
    (window-replace-content window)))

(fn window-content-scroll-up [window]
  (when (< 0 window.scroll-offset)
    (set window.scroll-offset (- window.scroll-offset 1))
    (window-replace-content window)))

(fn list-window-redraw-content [window]
  (local output [])

  (each [line-no item (ipairs (seq.copy (seq.take window.content window.content-height)))]
    (table.insert output (t.cursor.position.set_eq (- (+ window.start-r line-no) 1) window.start-c))
    (let [prefix (if (= line-no window.cursor) "> " "  ")
          line-chunk (.. prefix item.title)]
      (table.insert output line-chunk)))

  (t.output.write (stringx.join "" output))
  (t.output.flush))

(fn draw-window [window]
  (t.cursor.position.stack.push window.pos-r window.pos-c)

  (when window.focused
    (t.text.stack.push {:fg "yellow"}))

  (let [fmt (if window.focused my-focused-box-fmt my-box-fmt)]
    (t.draw.box window.height window.width fmt false window.title))
  (t.output.flush)

  (when window.focused
    (t.text.stack.pop))

  (t.cursor.position.stack.pop))

(fn main-screen []
  (t.cursor.visible.set false)

  (let [(rows cols) (sys.termsize)
        horizontal-sep (math.floor (* 2 (/ rows 3)))
        vertical-sep (math.floor (/ cols 3))
        events-window (new-window "1: Events"
                                  1 1
                                  horizontal-sep vertical-sep)
        details-window (new-list-window "2: Event Details" 
                                        1 (+ vertical-sep 1)
                                        horizontal-sep (- cols vertical-sep 1))
        variables-window (new-window "Variables"
                                     (+ horizontal-sep 1) 1
                                     (- rows horizontal-sep 1) (- cols 1))
        ; keybindings-window (new-window "Keybindings"
        ;                                (+ horizontal-sep 1) 1
        ;                                (- rows horizontal-sep 1) (- cols 1))
        windows [events-window details-window]]

    (set events-window.focused true)
    (local screen {})

    (fn screen.add-event [event]
      (list-window-add-item events-window event)
      (window-replace-content details-window
                              (inspect (. (list-window-active-item events-window)
                                          :content))))

    (fn screen.select-previous-event []
      (list-window-select-previous events-window)
      (set details-window.scroll-offset 0)
      (window-replace-content details-window
                              (inspect (. (list-window-active-item events-window)
                                          :content))))

    (fn screen.select-next-event []
      (list-window-select-next events-window)
      (set details-window.scroll-offset 0)
      (window-replace-content details-window
                              (inspect (. (list-window-active-item events-window)
                                          :content))))

    (fn screen.focus-window [target-window-id]
      (each [window-id window (ipairs windows)]
        (if (= window-id target-window-id)
          (set window.focused true)
          (set window.focused false))
        (draw-window window)))

    (fn screen.command [command]
      (when events-window.focused
        (case command
          :down (screen.select-next-event)
          :up (screen.select-previous-event)))
      (when details-window.focused
        (case command
          :down (window-content-scroll-down details-window)
          :up (window-content-scroll-up details-window))))

    (fn screen.add-variable [variable]
      (window-add-content-line variables-window (.. variable.name " <" variable.type "> = " variable.value)))

    (draw-window events-window)
    (draw-window details-window)
    (draw-window variables-window)
    ; (draw-window keybindings-window)
    ; (window-add-content-line keybindings-window "q: quit")
    ; (window-add-content-line keybindings-window "r: run")
    ; (window-add-content-line keybindings-window "c: continue")
    ; (window-replace-content details-window (stringx.splitlines (inspect details-window)))
    screen))

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
   :content nil})

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

(fn make-handler [screen send-request]
  (var seq 1)
  (fn seq-next []
    (let [seq-val seq]
      (set seq (+ 1 seq-val))
      seq-val))

  (fn request [...]
    (send-request (make-request (seq-next) ...)))

  (local handler {})

  (fn handler.handle-message [message]
    (screen.add-event
      {:title (.. message.content.type " " (or message.content.event message.content.command ""))
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
        (request :variables {:variablesReference scope.variablesReference}))

      [:response :variables]
      (each [_ variable (ipairs message.content.body.variables)]
        (screen.add-variable variable))))

  (fn handler.handle-command [command]
    (match command
      :run
      (request :initialize {:clientName "dap-tui" :adapterID "debugpy"})

      :continue
      (request :continue {})))

  handler)

(fn main []
  (t.initialize {:displaybackup true :filehandle io.stdout})
  (t.clear.screen)

  (local screen (main-screen))

  (local sock (copas.wrap (assert (socket.tcp))))
  (sock:connect "localhost" 5678)
  (sock:settimeout 0)

  (fn send-request [request]
    (let [content (cjson.decode request.content)]
      (screen.add-event
        {:title (.. "request" " " (or content.command ""))
         :content {:headers request.headers
                   :content content}}))
    (write-message sock request))

  (local handler (make-handler screen send-request))

  (var should-run? true)

  (copas.addthread
    (fn []
      (while should-run?
        (let [message (read-message sock)]
          (when message
            (handler.handle-message message))))))

  (copas.addthread
    (fn []
      (while should-run?
        (local (char typ sequence) (t.input.readansi math.huge copas.pause))
        (case char
          :1 (screen.focus-window 1)
          :2 (screen.focus-window 2)
          :r (handler.handle-command :run)
          :c (handler.handle-command :continue)

          ; :r (send-request (make-request 1000 :restart {}))
          ; :r (screen.redraw)

          :k (screen.command :up)
          :j (screen.command :down)

          :q (set should-run? false)

          _ (screen.add-event
              {:title "unhandled key pressed"
               :content {:char char
                         :type typ
                         :sequence sequence}})))))

  (while should-run?
    (set copas.running true)
    (copas.step))
  (set copas.running false)

  (t.shutdown))

(main)


; UI
; [done] version 1
; left pane - list of events coming from dap server
; right pane - view of each event
; bottom - list of keybindings
; controls
; - up/down/j/k - switch to next event
; - s/n/i/c - send event step into, next, initialize, continue?
;
; version 2
; C-h, C-l to switch between panes
; keybindings only effective in the active pane
; keybindings in popup window when ? is pressed


; TODO
; a better (reactive?) way to manage the UI updates
; helper context managers for terminal sequence building (fennel macros?)

; TODO
; distribution
; homebrew?

; TODO
; debug view - list of events + event details: display event as json formatted with jq
; actual view - source code, stack trace, variables

; TODO
; report bug in debugpy (connect argument being required in attach call)
