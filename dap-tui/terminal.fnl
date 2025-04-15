(local inspect (require "inspect"))
(local stringx (require "pl.stringx"))
(local sys (require "system"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))

(var -callback-binding nil)

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

(fn make-drawing-plan [layout size]
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
    (traverse-and-build layout {:row 1 :column 1} size))

  drawing-plan)

(fn events->variables [events]
  (->> (accumulate [variables {}
                    _ event (pairs events)]
         (do
           (case [(?. event :content :content :type)
                  (?. event :content :content :command)]
             [:response :variables]
             (each [_ variable (ipairs event.content.content.body.variables)]
               (tset variables variable.name variable.value)))
           variables))
       inspect
       (stringx.splitlines)))

(fn events->stack-trace [events]
  (accumulate [stack-trace []
               _ event (pairs events)]
    (let [content (?. event :content :content)]
      (case [(?. content :type)
             (?. content :command)]
        [:response :stackTrace]
        (each [_ frame (ipairs content.body.stackFrames)]
          (table.insert stack-trace
                        (.. frame.source.path ":" frame.line
                            " - " frame.name))))
      stack-trace)))

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
       inspect
       (stringx.splitlines)))

(fn make-tui []
  (local events (->cell []))
  (local screen-size (->cell (usable-termsize)))
  (local active-screen (->cell nil))
  (local active-window-key (->cell nil))
  (local active-event-no (->cell nil))
  (local active-event (->cell [events active-event-no]
                              (when (and events active-event-no)
                                (. events active-event-no))))

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
             :content [{:id :variables}
                       {:id :stack-trace}
                       {:id :breakpoint-details}]}})

  (local drawing-plan
    (->cell [active-screen screen-size]
      (let [layout (. layouts active-screen)]
        (make-drawing-plan layout screen-size))))

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

  (fn draw-lines [plan lines]
    (when plan
      (write
        (accumulate [str-so-far (.. (t.cursor.position.set_seq plan.location.row plan.location.column)
                                    (t.clear.box_seq plan.size.height plan.size.width))
                     line-no line (ipairs (or lines []))]
          (.. str-so-far
              (t.cursor.position.set_seq (- (+ line-no plan.location.row) 1)
                                         plan.location.column)
              line)))))

  (fn line-content-fn [lines]
    (fn [window]
      (let [window-plan window.plan
            content-plan (->cell [window-plan]
                                 (window-plan->content-plan window-plan))
            content (->cell [content-plan lines]
                            {:plan content-plan
                             :lines lines})]
        (->cell [content]
                (draw-lines content.plan content.lines)))))

  (fn make-window [id title params]
    (let [plan (->cell [drawing-plan] (. drawing-plan id))
          params (or params {})
          window-key (. params :key)
          title (if window-key
                  (.. (tostring window-key) ": " title)
                  title)
          focused? (->cell [active-window-key]
                           (and window-key (= window-key active-window-key)))
          border-data (->cell [focused? plan]
                              {:focused? focused?
                               :plan plan})
          border (->cell [border-data]
                         (draw-border border-data.plan title border-data.focused?))
          window {:id id
                  :title title
                  :params params
                  :plan plan
                  :border border}]

      (when params.content-fn
        (set window.content (params.content-fn window)))

      window))

  (fn make-event-list [id title params]
    (fn draw-event-item [item]
      (when item.content-plan
        (let [prefix (if item.active? "> " "  ")
              line-seq (t.cursor.position.set_seq (- (+ item.content-plan.location.row item.event-no) 1)
                                                  item.content-plan.location.column)]
          (write (.. (t.text.attr_seq {})
                     line-seq
                     (string.rep " " item.content-plan.size.width)
                     line-seq
                     prefix
                     item.event.label)))))

    (fn clear-below-events [plan event-count]
      (when plan
        (write
          (.. (t.cursor.position.set_seq (+ event-count plan.location.row) plan.location.column)
              (t.clear.box_seq (- plan.size.height event-count) plan.size.width)))))

    (let [window (make-window id title params)
          window-plan window.plan
          content-plan (->cell [window-plan]
                               (window-plan->content-plan window-plan))
          event-items []]

      (->cell [events]
              (each [event-no event (ipairs events)]
                (when (not (. event-items event-no))
                  (table.insert
                    event-items
                    (let [item (->cell [active-event-no content-plan]
                                       {:active? (= event-no active-event-no)
                                        :event-no event-no
                                        :event event
                                        :content-plan content-plan})]
                      (->cell [item]
                              (draw-event-item item))
                      item)))))
      (->cell [events content-plan]
              (clear-below-events content-plan (length events)))
      window))

  (local windows
    [(make-event-list :event-list "Event List"
                      {:key :1})
     (make-window :event-details "Event Details"
                  {:key :2
                   :content-fn (line-content-fn
                                 (->cell [active-event] 
                                         (when active-event
                                           (let [content-raw (?. active-event :content :content-raw) ]
                                             (stringx.splitlines (if content-raw
                                                                   (format-with-jq content-raw)
                                                                   (inspect active-event)))))))})
     (make-window :keybindings "Keybindings"
                  {:content-fn (line-content-fn
                                 (->cell ["q: quit"
                                          "r: run"
                                          "c: continue"
                                          "E/D: events view / debugger view"
                                          "1/2/...: Change focused window"]))})
     (make-window :variables "Variables"
                  {:content-fn (line-content-fn (->cell [events] (events->variables events)))})
     (make-window :stack-trace "Stack Trace"
                  {:content-fn (line-content-fn (->cell [events] (events->stack-trace events)))})
     (make-window :breakpoint-details "Breakpoint Details"
                  {:content-fn (line-content-fn (->cell [events] (events->breakpoint-details events)))})])

  (local tui {})

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
                       (active-window-key.set nil))

      :select-window (active-window-key.set (. params :window-key))

      :add-event (let [current-events (events.get)
                       next-events (tablex.copy current-events)]
                   (table.insert next-events params)
                   (events.set next-events)
                   (when (= 0 (length current-events))
                     (active-event-no.set 1)))

      :move-cursor (case (active-window-key.get)
                     :1 (let [events-count (length (events.get))
                              current-active-event-no (active-event-no.get)]
                          (when current-active-event-no
                            (case params.direction
                              :up (when (< 1 current-active-event-no)
                                    (active-event-no.set (- current-active-event-no 1)))
                              :down (when (< current-active-event-no events-count)
                                      (active-event-no.set (+ current-active-event-no 1)))))))))

  tui)

{:make-tui make-tui
 :usable-termsize usable-termsize}
