(local inspect (require "inspect"))
(local stringx (require "pl.stringx"))
(local sys (require "system"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))

(var -callback-binding nil)

(macro when-let [[s v & binds] & body]
  `(let [,s ,v]
     (when ,s
       (let [,(unpack binds)]
         ,(unpack body)))))

(fn make-cell [initial-value]
  (local cell {:value initial-value :callbacks {}})

  (fn cell.add-callback [callback]
    (when callback
      (set (. cell.callbacks callback) true)))

  (fn cell.get []
    (when -callback-binding
      (cell.add-callback -callback-binding))
    cell.value)

  (fn cell.set [new-value]
    (when (not (tablex.deepcompare new-value cell.value))
      (set cell.value new-value)
      (each [callback _ (pairs cell.callbacks)]
        (callback new-value)))
    new-value)

  cell)

(local -cell-count (make-cell 1))

(fn make-computed-cell [expr]
  (local cell (make-cell nil))
  (-cell-count.set (+ 1 (-cell-count.get)))

  (fn constructor []
    (expr))

  (fn callback []
    (cell.set (constructor)))

  (local previous-callback-binding -callback-binding)
  (set -callback-binding callback)
  (callback)
  (set -callback-binding previous-callback-binding)

  cell)

(macro ->cell [& args]
  (case args
    (where [deps & exprs] (and (< 0 (length exprs))
                               (sequence? deps)))
    `(make-computed-cell
       (fn []
          (let ,(accumulate [binds []
                             _ dep (ipairs deps)]
                  (do
                    (table.insert binds dep)
                    (table.insert binds `((. ,dep :get)))
                    binds))
            ,(unpack exprs))))

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

(fn write [data]
  (local debug false)
  (when data
    (local delay 0.05)
    (local batch-size 100)
    (local data-size (length data))
    (var ptr 1)
    (var tries nil)
    (if debug
      (do
        (t.cursor.visible.set true)
        (for [ptr 0 (length data)]
          (io.stdout:write (string.sub data ptr ptr))
          (io.stdout:flush)
          (t._bsleep 0.001)))

      (while (<= ptr data-size)
        (let [batch (string.sub data ptr (- (+ ptr batch-size) 1))
              (ok? error-message error-code) (io.stdout:write batch)]
          (if
            ok?
            (do
              (io.stdout:flush)
              (set ptr (+ ptr batch-size))
              (set tries nil))

            (or (= 11 error-code) (= 35 error-code))
            (do
              (set tries (+ 1 (or tries 0)))
              (io.stdout:flush)
              (os.exit 1)
              (t._bsleep (* tries delay)))

            (values ok? error-message error-code)))))))

(fn round [value]
  (let [(integral-part fractional-part) (math.modf value)]
    (if (< fractional-part 0.5)
      integral-part
      (+ 1 integral-part))))

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
                               (+ sum (or component.size 1)))]
    (collect [_ component (ipairs split.content)]
      (let [relative-size (or component.size 1)]
        (values component.id (round (* real-size (/ relative-size total-relative-sizes))))))))

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
  (->> (accumulate [breakpoint-details {}
                    _ event (pairs events)]
         (let [content (?. event :content :content)]
           (case [(?. content :type)
                  (?. content :event)]
             [:event :stopped]
             (do
               (tset breakpoint-details :reason content.body.reason)
               (tset breakpoint-details :description content.body.description)
               (tset breakpoint-details :text content.body.text)))
           breakpoint-details))
       inspect))

(fn make-tui []
  (local events (->cell []))
  (local screen-size (->cell (usable-termsize)))
  (local active-screen (->cell nil))
  (local active-window (->cell nil))
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
                                     (each [_ frame (ipairs content.body.stackFrames)]
                                       (table.insert frames frame))

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
  (local popup-window (->cell nil))

  (local layouts
    {:events {:type :horizontal-split
              :content [{:type :vertical-split
                         :id :left-sidebar
                         :content [{:id :event-list
                                    :size 4}
                                   {:id :keybindings}]}
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

  (local popup-window-layouts
    {:keybindings-popup {:id :keybindings-popup}
     :quit-dialog {:id :quit-dialog}})

  (local drawing-plan
    (->cell [active-screen screen-size]
            (let [layout (. layouts active-screen)
                  location {:row 1 :column 1}]
              (make-drawing-plan layout location screen-size))))

  (local popup-drawing-plan
    (->cell [popup-window screen-size]
            (when (and popup-window screen-size)
              (let [location {:row (round (* (/ 1 3) screen-size.height))
                              :column (round (* (/ 1 3) screen-size.width))}
                    size {:height (round (* (/ 1 3) screen-size.height))
                          :width (round (* (/ 1 3) screen-size.width))}
                    layout (. popup-window-layouts popup-window)]
                (. (make-drawing-plan layout location size) popup-window)))))

  (fn window-plan->content-plan [window-plan]
    (when window-plan
      {:size {:height (- window-plan.size.height 2)
              :width (- window-plan.size.width 2)}
       :location {:row (+ 1 window-plan.location.row)
                  :column (+ 1 window-plan.location.column)}}))

  (fn draw-border [plan title focused?]
    (when plan
      (write
        (.. (t.cursor.position.set_seq plan.location.row plan.location.column)
            (if focused? (t.text.attr_seq {:fg "yellow"}) (t.text.attr_seq {}))
            (t.draw.box_seq plan.size.height plan.size.width my-box-fmt false title)))))

  (fn pad-line [content line-plan]
    (.. (string.sub content 1 line-plan.size.width)
        (string.rep " " (- line-plan.size.width (length content)))))

  (fn intersect-with-popup [line-plan popup-plan content]
    (let [start line-plan.location.column
          end (+ start line-plan.size.width)
          popup-start (?. popup-plan :location :column)
          popup-end (when popup-start (+ popup-start popup-plan.size.width))
          full-line (pad-line content line-plan)
          no-intersect (.. (t.text.attr_seq {})
                           (t.cursor.position.set_seq line-plan.location.row line-plan.location.column)
                           full-line)]
      (if
        (not popup-plan)
        no-intersect

        (not (<= popup-plan.location.row line-plan.location.row (+ popup-plan.location.row popup-plan.size.height)))
        no-intersect

        (< start end popup-start popup-end)
        no-intersect

        (<= start popup-start end popup-end)
        (.. (t.text.attr_seq {})
            (t.cursor.position.set_seq line-plan.location.row line-plan.location.column)
            (string.sub full-line 1 (- popup-start start)))

        (< popup-start start end popup-end)
        ""

        (< popup-start start popup-end end)
        (.. (t.text.attr_seq {})
            (t.cursor.position.set_seq line-plan.location.row popup-end)
            (string.sub full-line popup-end))

        (< popup-end start)
        (.. (t.text.attr_seq {})
            (t.cursor.position.set_seq line-plan.location.row popup-end)
            full-line)

        (< start popup-start popup-end end)
        (.. (t.text.attr_seq {})
            (t.cursor.position.set_seq line-plan.location.row line-plan.location.column)
            (string.sub full-line 1 (- popup-start start))
            (t.cursor.position.set_seq line-plan.location.row popup-end)
            (string.sub full-line (+ 1 (- popup-end start)) line-plan.size.width)))))

  (fn make-window [id title params]
    (let [plan (->cell [drawing-plan]
                       (. drawing-plan id))
          params (or params {})
          window-key params.key
          title (if window-key
                  (.. (tostring window-key) ": " title)
                  title)
          focused? (->cell [active-window]
                           (= id active-window))
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
          content-lines []
          line-plans []
          printers []
          popup-intersects []
          window {:id id
                  :title title
                  :params params
                  :plan plan
                  :border border}]

      (->cell (when-let [content-plan2 (content-plan.get)]
                (let [content-cell-lines2 (content-cell-lines.get)]
                  (for [line-no (+ 1 (length content-lines)) (math.max (length content-cell-lines2)
                                                                       content-plan2.size.height)]
                    (let [content-line (->cell [content-cell-lines]
                                               (or (. content-cell-lines line-no) ""))]
                      (table.insert content-lines content-line))))

                (for [line-no (+ 1 (length line-plans)) content-plan2.size.height]
                  (let [plan (->cell [content-plan]
                                     (when (and content-plan (<= line-no content-plan.size.height))
                                       {:size {:height 1 :width content-plan.size.width}
                                        :location {:row (- (+ line-no content-plan.location.row) 1)
                                                   :column content-plan.location.column}}))]
                    (table.insert line-plans plan)))

                (for [line-no (+ 1 (length printers)) content-plan2.size.height]
                  (when-let [content-line (. content-lines line-no)
                             line-plan (. line-plans line-no)]
                    (let [popup-intersect (->cell [line-plan content-line popup-drawing-plan]
                                                  (when line-plan
                                                    (intersect-with-popup line-plan popup-drawing-plan content-line)))
                          printer (->cell [popup-intersect]
                                          (write popup-intersect))]
                      (table.insert popup-intersects popup-intersect)
                      (table.insert printers printer))))))
      window))

  (fn make-popup-window [id title params]
    (let [border (->cell [popup-drawing-plan popup-window]
                         (when (= id popup-window)
                           (draw-border popup-drawing-plan title false)))
          content-plan (->cell [popup-drawing-plan popup-window]
                               (when (and popup-drawing-plan (= id popup-window))
                                 (window-plan->content-plan popup-drawing-plan)))
          content-cell (or params.content-cell (->cell ""))
          content-cell-lines (->cell [content-cell]
                                     (stringx.splitlines (or content-cell "")))
          content-lines []
          line-plans []
          printers [] ]

      (->cell (when-let [content-plan2 (content-plan.get)]
                (let [content-cell-lines2 (content-cell-lines.get)]
                  (for [line-no (+ 1 (length content-lines)) (math.max (length content-cell-lines2)
                                                                       content-plan2.size.height)]
                    (let [content-line (->cell [content-cell-lines]
                                               (or (. content-cell-lines line-no) ""))]
                      (table.insert content-lines content-line))))

                (for [line-no (+ 1 (length line-plans)) content-plan2.size.height]
                  (let [plan (->cell [content-plan]
                                     (when (and content-plan (<= line-no content-plan.size.height))
                                       {:size {:height 1 :width content-plan.size.width}
                                        :location {:row (- (+ line-no content-plan.location.row) 1)
                                                   :column content-plan.location.column}}))]
                    (table.insert line-plans plan)))

                (for [line-no (+ 1 (length printers)) content-plan2.size.height]
                  (when-let [content-line (. content-lines line-no)
                             line-plan (. line-plans line-no)]
                    (let [printer (->cell [line-plan content-line]
                                          (when line-plan
                                            (write (.. (t.text.attr_seq {})
                                                       (t.cursor.position.set_seq line-plan.location.row line-plan.location.column)
                                                       (pad-line content-line line-plan)))))]
                      (table.insert printers printer))))))

      {:border border
       :id id
       :title title}))

  (local windows-
    [(make-popup-window :keybindings-popup "Keybindings"
                        {:content-cell (->cell
                                         (->> ["q: quit"
                                               "r: run"
                                               "c: continue"
                                               "E/D: events view / debugger view"
                                               "1/2/...: Change focused window"]
                                              (stringx.join "\n")))})
     (make-popup-window :quit-dialog "Quit?"
                        {:content-cell (->cell
                                         (->> ["Quit?"
                                               "[y]es / [n]o"]
                                              (stringx.join "\n")))})
     (make-window :event-list "Event List"
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
                                               "E/D: events view / debugger view"
                                               "1/2/...: Change focused window"]
                                              (stringx.join "\n")))})
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
                                                (stringx.join "\n")))
                                         )})
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
                                               (let [lines (. event.content active-frame.source.path)]
                                                 (->> (icollect [line-no line (ipairs lines)]
                                                        (let [prefix (if (= line-no active-frame.line) ">> " "   ")]
                                                          (.. prefix line)))
                                                      (stringx.join "\n")))
                                               source))))})
     (make-window :breakpoint-details "Breakpoint Details"
                  {:content-cell (->cell [events]
                                         (events->breakpoint-details events))})])

  (local windows (collect [_ window (ipairs windows-)]
                   (values window.id window)))


  (local tui {})
  (->cell [popup-window]
          (set tui.popup-window popup-window))

  (fn tui.initialize []
    (t.initialize {:displaybackup true :filehandle io.stdout})
    (t.cursor.visible.set false)
    (set tui.initialized true)
    (t.clear.screen)

    (active-screen.set :events))

  (fn tui.shutdown []
    (t.shutdown))

  (fn tui.handle-command [command params]
    (case command
      :set-screensize (screen-size.set params)

      :select-screen (do
                       (active-screen.set params.screen-id)
                       (active-window.set nil))

      :select-window (accumulate [selected-window-id nil
                                  window-id window (pairs windows)]
                       (active-window.set
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
                   (when (and (not (active-frame-no.get))
                              (= :response (?. params :content :content :type))
                              (= :stackTrace (?. params :content :content :command)))
                     (active-frame-no.set 1)))

      :move-cursor (case (active-window.get)
                     :event-list (when-let [current-active-event-no (active-event-no.get)
                                            events-count (length (events.get))]
                                   (case params.direction
                                     :up (when (< 1 current-active-event-no)
                                           (active-event-no.set (- current-active-event-no 1)))
                                     :down (when (< current-active-event-no events-count)
                                             (active-event-no.set (+ current-active-event-no 1)))))
                     :stack-trace (when-let [current-active-frame-no (active-frame-no.get)
                                             frame-count (length (stack-trace.get))]
                                    (case params.direction
                                      :up (when (< 1 current-active-frame-no)
                                            (active-frame-no.set (- current-active-frame-no 1)))
                                      :down (when (< current-active-frame-no frame-count)
                                              (active-frame-no.set (+ current-active-frame-no 1))))))

      :toggle-popup (let [window-id params.window-id]
                      (if (not= (popup-window.get) window-id)
                        (popup-window.set params.window-id)
                        (popup-window.set nil)))))

  tui)

{:make-tui make-tui
 :usable-termsize usable-termsize}
