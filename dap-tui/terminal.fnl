(local inspect (require "inspect"))
(local stringx (require "pl.stringx"))
(local sys (require "system"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))

(macro when-let [[s v & binds] & body]
  `(let [,s ,v]
     (when ,s
       (let [,(unpack binds)]
         ,(unpack body)))))

(var *callback-binding* nil)

(fn make-cell [initial-value]
  (local cell {:value initial-value
               :callbacks {}})

  (fn cell.add-callback [callback]
    (when callback
      (set (. cell.callbacks callback) true)))

  (fn cell.get []
    (when *callback-binding*
      (cell.add-callback *callback-binding*))
    cell.value)

  (fn cell.broadcast [_ old-value]
    (each [callback _ (pairs cell.callbacks)]
      (callback cell cell.value old-value)))

  (fn cell.set [new-value]
    (when (not (tablex.deepcompare new-value cell.value))
      (let [old-value cell.value]
        (set cell.value new-value)
        (cell.broadcast new-value old-value)))
    new-value)

  cell)

(fn make-computed-cell [expr]
  (local cell (make-cell nil))

  (fn callback [initiator initiator-new-value initiator-old-value]
    (cell.set (expr initiator initiator-new-value initiator-old-value)))

  (local previous-callback-binding *callback-binding*)
  (set *callback-binding* callback)
  (callback cell)
  (set *callback-binding* previous-callback-binding)
  cell)

(macro ->cell [& args]
  (case args
    (where [dep-syms & exprs] (and (< 0 (length exprs))
                                   (sequence? dep-syms)))
    (let [dep->name# (collect [_# dep# (ipairs dep-syms)]
                       (values dep# (tostring dep#)))
          binds# (accumulate [binds# []
                              _# dep# (ipairs dep-syms)]
                   (do
                     (table.insert binds# dep#)
                     (table.insert binds# `((. ,dep# :get)))
                     binds#))]
      `(make-computed-cell
         (fn [initiator-cell# ,(sym :*initiator-new-value*) ,(sym :*initiator-old-value*)]
            (let [,(sym :*initiator*) (. ,dep->name# initiator-cell#)]
              (let ,binds#
                ,(unpack exprs))))))

    (where [& exprs] (< 0 (length exprs)))
    `(make-computed-cell (fn [] ,(unpack exprs)))

    _
    (assert-compile false "At least one argument is needed")))

(fn format-with-jq [data]
  (let [jq (io.popen "jq '.' > /tmp/dap-tui-jq-output.json" "w")]
    (jq:write data)
    (jq:flush)
    (jq:close))
  (let [jq-output (io.open "/tmp/dap-tui-jq-output.json" "r")
        formatted (jq-output:read "a")]
    (jq-output:close)
    formatted))

(fn make-writer []
  (local writer {:chunks []})

  (fn writer.reset []
    (set (. writer :chunks) []))

  (fn writer.write [chunk]
    (table.insert writer.chunks chunk))

  (fn writer.flush [slow?]
    (when (< 0 (length writer.chunks))
      (when slow?
        (t.cursor.visible.set true))

      (let [data (stringx.join "" writer.chunks)]
        (if slow?
          (for [ptr 1 (length data)]
            (t.output.write (string.sub data ptr ptr))
            (t.output.flush)
            (t._bsleep 0.005))
          (do
            (t.output.write data)
            (t.output.flush))))

      (when slow?
        (t.cursor.visible.set false))

      (writer.reset)))

  writer)

(fn pad-line [content width]
  (.. (string.sub content 1 width)
      (string.rep " " (- width (length content)))))

(fn left-pad [value total-length pad-char]
  (let [pad-char (or pad-char " ")
        value-str (tostring value)
        pad-size (- total-length (length value-str))]
    (.. (string.rep pad-char pad-size) value-str)))

(fn different-section [new old]
  (let [max-length (math.max (length new) (length old))
        new-str (pad-line new max-length)
        old-str (pad-line old max-length)]
    (var first-different-loc 1)
    (var last-different-loc max-length)

    (for [loc max-length 1 -1]
      (let [new-char (string.sub new-str loc loc)
            old-char (string.sub old-str loc loc)]
        (when (not= new-char old-char)
          (set first-different-loc loc))))

    (for [loc 1 max-length]
      (let [new-char (string.sub new-str loc loc)
            old-char (string.sub old-str loc loc)]
        (when (not= new-char old-char)
          (set last-different-loc loc))))

    (values first-different-loc last-different-loc)))

(fn usable-termsize []
  ;; When drawing a box all the way to the edge it glitches out
  (let [(height width) (sys.termsize)]
    {:width (- width 1)
     :height height}))

(local my-box-fmt
  (tablex.union t.draw.box_fmt.single
                {:post " " :pre " "}))

(local my-focused-box-fmt
  (tablex.union t.draw.box_fmt.double
                {:post " " :pre " "}))

(fn split-real-sizes [split real-size]
  (let [total-relative-sizes (accumulate [sum 0
                                          _ component (ipairs split.content)]
                               (+ sum (or component.size 1)))
        sizes (collect [_ component (ipairs split.content)]
                (values component.id
                        (math.floor (* real-size (/ (or component.size 1) total-relative-sizes)))))
        total-floored (accumulate [sum 0
                                   _ size (pairs sizes)]
                        (+ sum size))
        remaining-space (math.max 0 (- real-size total-floored))
        sorted-sizes (icollect [component-id size (tablex.sortv sizes)]
                       {: component-id : size})
        padding (fcollect [_ 1 remaining-space] 1)]
    (accumulate [sizes sizes
                 _ [{: component-id : size} pad] (pairs (tablex.zip sorted-sizes padding))]
      (do
        (set (. sizes component-id) (+ size pad))
        sizes))))

(fn make-drawing-plan [layout location size]
  (local drawing-plan {})

  (fn traverse-and-build [component-layout location size]
    (case component-layout.type
      :container
      (let [(next content) (ipairs component-layout.content)
            (_ subcomponent-layout) (next content)]
        (traverse-and-build subcomponent-layout location size))

      :horizontal-split
      (let [sizes (split-real-sizes component-layout size.width)]
        (accumulate [location2 location
                     _ subcomponent (ipairs component-layout.content)]
          (let [size2 {:width (. sizes subcomponent.id)
                       :height size.height}
                next-location {:row location2.row
                               :column (+ location.column size2.width)}]
            (traverse-and-build subcomponent location2 size2)
            next-location)))

      :vertical-split
      (let [sizes (split-real-sizes component-layout size.height)]
        (accumulate [location2 location
                     _ subcomponent (ipairs component-layout.content)]
          (let [size2 {:width size.width
                       :height (. sizes subcomponent.id)}
                next-location {:row (+ location2.row size2.height)
                               :column location.column}]
            (traverse-and-build subcomponent location2 size2)
            next-location)))

      _ (set (. drawing-plan component-layout.id) {:location location :size size})))

  (when layout
    (traverse-and-build layout location size))

  drawing-plan)

(fn events->breakpoint-details [events]
  (->> (accumulate [breakpoint-details nil
                    _ event (pairs events)]
         (let [content (?. event :content :content)]
           (case [(?. content :type) (?. content :event)]
             [:event :stopped]
             {:reason content.body.reason
              :description content.body.description
              :text content.body.text})
           breakpoint-details))
       inspect))

(fn make-tui []
  (local tui {:writer (make-writer)})
  (local slow-write? (->cell false))

  (local events (->cell []))
  (local screen-size (->cell (usable-termsize)))
  (local active-screen (->cell nil))
  (local active-window-id (->cell nil))
  (->cell [active-window-id] (set (. tui :active-window) active-window-id))
  (local active-event-no (->cell nil))
  (local active-event (->cell [events active-event-no]
                              (when (and events active-event-no)
                                (. events active-event-no))))
  (local stack-trace (->cell [events]
                             (let [frames []
                                   frames-by-request-seq {}
                                   scopes-by-frame-id {}
                                   variables-requests-by-scope-variable-reference {}
                                   variables-responses-by-request-seq {}]
                               (each [_ event (pairs events)]
                                 (let [content (?. event :content :content)]
                                   (case [(?. content :type) (?. content :command)]
                                     [:response :stackTrace]
                                     (do
                                       (tablex.clear frames)
                                       (each [_ frame (ipairs content.body.stackFrames)]
                                         (table.insert frames frame)))

                                     [:request :scopes]
                                     (set (. frames-by-request-seq content.seq) content.arguments.frameId)

                                     [:response :scopes]
                                     (let [frame-id (. frames-by-request-seq content.request_seq)]
                                       (set (. scopes-by-frame-id frame-id) content.body.scopes))

                                     [:request :variables]
                                     (set (. variables-requests-by-scope-variable-reference content.arguments.variablesReference) content)

                                     [:response :variables]
                                     (set (. variables-responses-by-request-seq content.request_seq) content.body.variables))))
                               (each [_ frame (ipairs frames)]
                                 (set (. frame :event-count) (length events))
                                 (set (. frame :scopes) [])
                                 (each [scope-no scope (ipairs (or (. scopes-by-frame-id frame.id) []))]
                                   (table.insert frame.scopes scope)
                                   (when-let [variables-request (. variables-requests-by-scope-variable-reference scope.variablesReference)]
                                     (when-let [variables-response (. variables-responses-by-request-seq variables-request.seq)]
                                       (set (. scope :variables) variables-response)))))
                               frames)))
  (local active-frame-no (->cell nil))
  (local active-frame (->cell [stack-trace active-frame-no]
                              (when (and stack-trace active-frame-no)
                                (. stack-trace active-frame-no))))

  (local layouts
    {:events {:type :horizontal-split
              :content [{:type :vertical-split
                         :id :left-sidebar
                         :content [{:id :event-list
                                    :size 6}
                                   {:id :keybindings}
                                   {:id :info}]}
                        {:id :event-details
                         :size 4}]}
     :debug {:type :vertical-split
             :content [{:type :horizontal-split
                        :id :debug-upper-section
                        :content [{:id :breakpoint-details}
                                  {:id :variables}]}
                       {:type :horizontal-split
                        :id :debug-lower-section
                        :size 3
                        :content [{:id :stack-trace}
                                  {:id :source
                                   :size 3}]}]}})

  (local drawing-plan
    (->cell [active-screen screen-size]
            (let [layout (. layouts active-screen)
                  location {:row 1 :column 1}]
              (make-drawing-plan layout location screen-size))))

  (fn window-plan->content-plan [window-plan]
    (when window-plan
      {:size {:height (- window-plan.size.height 2)
              :width (- window-plan.size.width 2)}
       :location {:row (+ 1 window-plan.location.row)
                  :column (+ 1 window-plan.location.column)}}))

  (fn draw-border [plan title focused?]
    (when plan
      (tui.writer.write
        (.. (t.cursor.position.set_seq plan.location.row plan.location.column)
            (if focused? (t.text.attr_seq {:fg "yellow"}) (t.text.attr_seq {}))
            (t.draw.box_seq plan.size.height plan.size.width my-box-fmt false title)))))

  (fn make-window [id title params]
    (let [plan (->cell [drawing-plan]
                       (. drawing-plan id))
          params (or params {})
          window-key params.key
          title (if window-key
                  (.. (tostring window-key) ": " title)
                  title)
          focused? (->cell [active-window-id]
                           (= id active-window-id))
          border-data (->cell [focused? plan]
                              {:focused? focused?
                               :plan plan})
          border (->cell [border-data]
                         (draw-border border-data.plan title border-data.focused?))
          content-plan (->cell [plan]
                               (window-plan->content-plan plan))
          content-cell (or params.content-cell (->cell ""))
          content-cell-lines (->cell [content-cell]
                                     (stringx.splitlines (or content-cell "")))
          window-content []
          scroll {:horizontal (->cell 0)
                  :vertical (->cell 0)}
          window {: id
                  : title
                  : params
                  : plan
                  : content-plan
                  : border
                  : scroll}]

      (content-plan.add-callback
        (fn [_ content-plan2]
          (when content-plan2
            (scroll.vertical.set 0)
            (scroll.horizontal.set 0)
            (let [content-cell-lines2 (content-cell-lines.get)]
              (for [line-no (+ 1 (length window-content)) content-plan2.size.height]
                (let [plan (->cell [content-plan]
                                   (when (and content-plan (<= line-no content-plan.size.height))
                                     {:size {:height 1
                                             :width content-plan.size.width}
                                      :location {:row (- (+ line-no content-plan.location.row) 1)
                                                 :column content-plan.location.column}}))
                      scroll-horizontal scroll.horizontal
                      scroll-vertical scroll.vertical
                      content (->cell [content-cell-lines scroll-vertical scroll-horizontal]
                                      (string.sub (or (. content-cell-lines (+ line-no scroll-vertical)) "")
                                                   (+ 1 scroll-horizontal)))]
                  (->cell [plan content]
                          (when plan
                            (tui.writer.write
                              (if (= :content *initiator*)
                                (let [(start end) (different-section content *initiator-old-value*)
                                      max-width (math.max (length content) (length *initiator-old-value*))]
                                  (.. (t.text.attr_seq {})
                                      (t.cursor.position.set_seq plan.location.row (- (+ plan.location.column start) 1))
                                      (-> (pad-line content max-width)
                                          (string.sub start (math.min end plan.size.width)))))
                                (.. (t.text.attr_seq {})
                                    (t.cursor.position.set_seq plan.location.row plan.location.column)
                                    (pad-line content plan.size.width))))))
                  (table.insert window-content {: plan : content})))))))

      (fn window.scroll-up []
        (let [scroll-vertical (scroll.vertical.get)]
          (when (< 0 scroll-vertical)
            (scroll.vertical.set (- scroll-vertical 1)))))

      (fn window.scroll-down []
        (when-let [plan (content-plan.get)
                   lines (content-cell-lines.get)
                   scroll-vertical (scroll.vertical.get)
                   overflow (- (length lines) plan.size.height)]
          (when (< scroll-vertical overflow)
            (scroll.vertical.set (+ scroll-vertical 1)))))

      (fn window.scroll-left []
        (let [scroll-horizontal (scroll.horizontal.get)]
          (when (< 0 scroll-horizontal)
            (scroll.horizontal.set (- scroll-horizontal 1)))))

      (fn window.scroll-right []
        (when-let [plan (content-plan.get)
                   longest-line (accumulate [max-length 0
                                             _ line (ipairs (content-cell-lines.get))]
                                  (math.max max-length (length (stringx.rstrip line))))
                   scroll-horizontal (scroll.horizontal.get)
                   overflow (- longest-line plan.size.width)]
          (when (< scroll-horizontal overflow)
            (scroll.horizontal.set (+ scroll-horizontal 1)))))

      (fn window.scroll-top []
        (scroll.vertical.set 0))

      (fn window.scroll-bottom []
        (when-let [plan (content-plan.get)
                   lines (content-cell-lines.get)
                   overflow (- (length lines) plan.size.height)]
          (when (< 0 overflow)
            (scroll.vertical.set overflow))))

      window))

  (local windows-
    [(make-window :event-list "Event List"
                  {:key :1
                   :content-cell (->cell [events active-event-no]
                                         (->> (icollect [event-no event (ipairs events)]
                                                (let [active? (= event-no active-event-no)
                                                      prefix (if active? "> " "  ")]
                                                  (.. prefix event.label)) )
                                              (stringx.join "\n")))})
     (make-window :event-details "Event Details"
                  {:key :2
                   :content-cell (->cell [active-event]
                                         (when active-event
                                           (let [content-raw (?. active-event :content :content-raw)]
                                             (if content-raw
                                               (format-with-jq content-raw)
                                               (inspect active-event)))))})
     (make-window :keybindings "Keybindings"
                  {:content-cell (->cell (->> ["q: quit"
                                               "r: run"
                                               "c: continue"
                                               "h/j/k/l: move cursor / scroll"
                                               "gg/G: jump to top / jump to bottom"
                                               "E/D: events view / debugger view"
                                               "1/2/...: Change focused window"]
                                              (stringx.join "\n")))})
     (make-window :info "Info"
                  {:content-cell (->cell [events slow-write?]
                                         (when events
                                           (->> [(.. "Events: " (tostring (length events)))
                                                 (.. "Slow write enabled: " (tostring slow-write?))]
                                                (stringx.join "\n"))))})
     (make-window :variables "Variables"
                  {:content-cell (->cell [active-frame stack-trace]
                                         (when active-frame
                                           (->> (accumulate [lines []
                                                             _ scope (ipairs active-frame.scopes)]
                                                  (do
                                                    (table.insert lines scope.name)
                                                    (each [_ variable (ipairs (or scope.variables []))]
                                                      (when (not= "" variable.type)
                                                        (table.insert lines (.. "  " variable.name
                                                                                " <" variable.type ">"
                                                                                " = " variable.value))))
                                                    lines))
                                                (stringx.join "\n"))))})
     (make-window :stack-trace "Stack Trace"
                  {:key :1
                   :content-cell (->cell [stack-trace active-frame-no]
                                         (->> (icollect [frame-no frame (ipairs stack-trace)]
                                                (.. (if (= active-frame-no frame-no) "> " "  ")
                                                    frame.source.path ":" frame.line
                                                    " - " frame.name))
                                              (stringx.join "\n")))})
     (make-window :source "Source"
                  {:content-cell (->cell [events active-frame]
                                         (when active-frame
                                           (accumulate [source ""
                                                        _ event (ipairs events)]
                                             (if (= :sources-loaded event.type)
                                               (let [lines (. event.content active-frame.source.path)
                                                     max-number-length (-> lines length tostring length)]
                                                 (->> (icollect [line-no line (ipairs lines)]
                                                        (let [prefix (if (= line-no active-frame.line) ">> " "   ")]
                                                          (.. prefix
                                                              (left-pad line-no (+ 1 max-number-length))
                                                              " "
                                                              line)))
                                                      (stringx.join "\n")))
                                               source))))})
     (make-window :breakpoint-details "Breakpoint Details"
                  {:content-cell (->cell [events]
                                         (events->breakpoint-details events))})])

  (local windows (collect [_ window (ipairs windows-)]
                   (values window.id window)))

  (local active-window (->cell [active-window-id]
                               (. windows active-window-id)))

  (fn tui.initialize []
    (t.initialize {:displaybackup true :filehandle io.stdout})
    (t.cursor.visible.set false)
    (set tui.initialized true)
    (t.clear.screen)

    (active-screen.set :events)
    (tui.writer.flush (slow-write?.get)))

  (fn tui.shutdown []
    (t.shutdown))

  (fn tui.handle-command [command params]
    (tui.writer.reset)
    (case command
      :set-screensize (screen-size.set params)

      :toggle-slow-write (slow-write?.set (not (slow-write?.get)))

      :select-screen (do
                       (active-screen.set params.screen-id)
                       (active-window-id.set nil))

      :select-window (accumulate [selected-window-id (active-window-id.get)
                                  window-id window (pairs windows)]
                       (active-window-id.set
                         (if (and window.plan
                                  (window.plan.get)
                                  (= (?. window :params :key) params.window-key))
                           window-id
                           selected-window-id)))

      :add-event (let [new-events (tablex.deepcopy (events.get))]
                   (table.insert new-events params)
                   (events.set new-events)

                   (when (= 1 (length new-events))
                     (active-event-no.set 1))

                   (when (and (= :response (?. params :content :content :type))
                              (= :stackTrace (?. params :content :content :command)))
                     (let [trace-size (length params.content.content.body.stackFrames)]
                       (if
                         (not (active-frame-no.get))
                         (active-frame-no.set 1)

                         (< trace-size (active-frame-no.get))
                         (active-frame-no.set trace-size)))))

      :move-cursor (do
                     (when-let [window (active-window.get)]
                       (case params.direction
                         :left (window.scroll-left)
                         :right (window.scroll-right)
                         :top (window.scroll-top)
                         :bottom (window.scroll-bottom)))

                     (case tui.active-window
                       :event-list (when-let [event-no (active-event-no.get)
                                              events-count (length (events.get))
                                              window (active-window.get) 
                                              content-plan (window.content-plan.get)
                                              window-height content-plan.size.height]
                                     (case params.direction
                                       :top (active-event-no.set 1)
                                       :bottom (active-event-no.set events-count)
                                       :up (do
                                             (when (< 1 event-no)
                                               (active-event-no.set (- event-no 1)))
                                             (when (= event-no (+ 1 (window.scroll.vertical.get)))
                                               (window.scroll-up)))
                                       :down (do
                                               (when (< event-no events-count)
                                                 (active-event-no.set (+ event-no 1)))
                                               (when (>= event-no window-height)
                                                 (window.scroll-down)))))
                       :stack-trace (when-let [current-active-frame-no (active-frame-no.get)
                                               frame-count (length (stack-trace.get))]
                                      (case params.direction
                                        :top (active-frame-no.set 1)
                                        :bottom (active-frame-no.set frame-count)
                                        :up (when (< 1 current-active-frame-no)
                                              (active-frame-no.set (- current-active-frame-no 1)))
                                        :down (when (< current-active-frame-no frame-count)
                                                (active-frame-no.set (+ current-active-frame-no 1)))))
                       _ (when-let [window (active-window.get)]
                           (case params.direction
                             :up (window.scroll-up)
                             :down (window.scroll-down))))))

    (tui.writer.flush (slow-write?.get)))

  tui)

{: make-tui
 : usable-termsize}
