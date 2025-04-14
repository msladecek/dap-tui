(local inspect (require "inspect"))
(local stringx (require "pl.stringx"))
(local sys (require "system"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))

;; TODO: bottom toolbar with basic debug info (eg. cell count)

;; TODO: sanitize this using macros gensyms
(var -callback-binding nil)

;; TODO:
; 1. register all the dependencies of a cell
; 2. register all child cells (cells defined within the expr of the parent cell)
; 3. when the value changes unhook all child cells from their dependencies

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

(macro ->cell [...]
  (let [expr `(fn [] ,(do ...))]
    `(make-computed-cell ,expr)))

(macro on-change-of [cell callback-args callback-body]
  (let [callback `(fn ,callback-args ,callback-body)]
    `((. ,cell :add-callback) ,callback)))

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
  (local active-event (->cell (let [events (events.get)
                                    event-no (active-event-no.get)]
                                (when (and events event-no)
                                  (. events event-no)))))

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
    (->cell
      (let [layout (. layouts (active-screen.get))]
        (make-drawing-plan layout (screen-size.get)))))

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
      (let [content-plan (->cell (window-plan->content-plan (window.plan.get)))
            content-data (->cell {:plan (content-plan.get)
                                  :lines (lines.get)})]
        (->cell (let [content (content-data.get)]
                  (draw-lines content.plan content.lines))))))

  (fn make-window [id title params]
    ;; TODO: keep a list of cells, one per line
    ;;       when drawing, instead of outputing ascii, set text in a specific line
    (let [plan (->cell (. (drawing-plan.get) id))
          params (or params {})
          window-key (. params :key)
          title (if window-key
                  (.. (tostring window-key) ": " title)
                  title)
          focused? (->cell (and window-key (= window-key (active-window-key.get))))
          border-data (->cell {:focused? (focused?.get)
                               :plan (plan.get)})
          border (->cell
                   (let [border (border-data.get)]
                     (draw-border border.plan title border.focused?)))
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
          content-plan (->cell (window-plan->content-plan (window.plan.get)))
          event-items []]

      (->cell (each [event-no event (ipairs (events.get))]
                (when (not (. event-items event-no))
                  (table.insert
                    event-items
                    (let [item-data (->cell {:active? (= event-no (active-event-no.get))
                                             :event-no event-no
                                             :event event
                                             :content-plan (content-plan.get)})]
                      (->cell (let [item (item-data.get)]
                                (draw-event-item item)))
                      item-data)))))

      (->cell (let [events (events.get)
                    plan (content-plan.get)]
                (clear-below-events plan (length events))))

      window))

  (local windows
    [(make-event-list :event-list "Event List"
                      {:key :1})
     (make-window :event-details "Event Details"
                  {:key :2
                   :content-fn (line-content-fn
                                 (->cell (let [event (active-event.get)]
                                           (when event
                                             (let [content-raw (?. event :content :content-raw) ]
                                               (stringx.splitlines (if content-raw
                                                                     (format-with-jq content-raw)
                                                                     (inspect event))))))))})
     (make-window :keybindings "Keybindings"
                  {:content-fn (line-content-fn
                                 (->cell ["q: quit"
                                          "r: run"
                                          "c: continue"]))})
     (make-window :variables "Variables"
                  {:content-fn (line-content-fn (->cell (events->variables (events.get))))})
     (make-window :stack-trace "Stack Trace"
                  {:content-fn (line-content-fn (->cell (events->stack-trace (events.get))))})
     (make-window :breakpoint-details "Breakpoint Details"
                  {:content-fn (line-content-fn (->cell (events->breakpoint-details (events.get))))})])

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
                                    (active-event-no.set (+ current-active-event-no 1)))))))
    ))

tui)

{:make-tui make-tui
 :usable-termsize usable-termsize}
