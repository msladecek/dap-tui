(local inspect (require "inspect"))
(local stringx (require "pl.stringx"))
(local sys (require "system"))
(local t (require "terminal"))
(local tablex (require "pl.tablex"))

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
  (when data
    (local delay 0.05)
    (local batch-size 100)
    (local data-size (length data))
    (var ptr 1)
    (var tries nil)
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
            (t._bsleep (* tries delay)))
          (values ok? error-message error-code))))))

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

(fn make-tui []
  (local tui
    {:events []
     :initialized false
     :size (usable-termsize)
     :active {:screen :debug
              :window :event-list
              :event-list--event nil}
     :layouts {:events {:type :horizontal-split
                        :order [:left-sidebar :event-details]
                        :ratios {:left-sidebar 1 :event-details 4}
                        :content {:left-sidebar {:type :vertical-split
                                                 :order [:event-list :keybindings]
                                                 :ratios {:event-list 4 :keybindings 1}
                                                 :content {:event-list {} :keybindings {}}}
                                  :event-details {}}}
               :debug {:type :vertical-split
                       :order [:variables :stack-trace :breakpoint-details]
                       :ratios {:variables 1 :stack-trace 1 :breakpoint-details 1}
                       :content {:variables {}
                                 :stack-trace {}
                                 :breakpoint-details {}}}}
     :location-plan {}
     :components {:event-list {:title "Events"
                               :key :1
                               :type :list
                               :scroll-offset {:row 0 :column 0}}
                  :event-details {:title "Event Details"
                                  :key :2
                                  :scroll-offset {:row 0 :column 0}}
                  :keybindings {:title "Keybindings"
                                :scroll-offset {:row 0 :column 0}}
                  :stack-trace {:title "Stack Trace"
                                :scroll-offset {:row 0 :column 0}}
                  :variables {:title "Variables"
                              :scroll-offset {:row 0 :column 0}}
                  :breakpoint-details {:title "Breakpoint Details"
                                       :scroll-offset {:row 0 :column 0}}}})

  (fn split-real-sizes [split real-size]
    (let [ratios-sum (accumulate [sum 0
                                  _ ratio (pairs split.ratios)]
                       (+ sum ratio))]
      (collect [subcomponent-id subcomponent-ratio (pairs split.ratios)]
        (values subcomponent-id (round (* real-size (/ subcomponent-ratio ratios-sum)))))))

  (fn window-content-term-seq [component-id location size]
    (let [content-size (tablex.map (fn [s] (- s 2)) size)
          content-location (tablex.map (fn [l] (+ 1 l)) location)
          scroll-offset (. tui.components component-id :scroll-offset)
          content-lines (match component-id
                          :event-list (icollect [_ event (ipairs tui.events)]
                                        (let [prefix (if event.selected "> " "  ")]
                                          (.. prefix event.label)))
                          :event-details (if tui.active.event-list--event
                                           (let [active-event (. tui.events tui.active.event-list--event)
                                                 text (or active-event.content.content-formatted
                                                          (inspect active-event.content))
                                                 lines (stringx.splitlines text)]
                                             lines)
                                           [])
                          :keybindings ["q: quit"
                                        "E: events view"
                                        "D: debugging view"
                                        "r: run"
                                        "c: continue"
                                        "R: redraw"]
                          :variables (->> (accumulate [variables {}
                                                       _ event (pairs tui.events)]
                                            (do
                                              (match [(?. event :content :content :type)
                                                      (?. event :content :content :command)]
                                                [:response :scopes]
                                                nil

                                                [:response :variables]
                                                (each [_ variable (ipairs event.content.content.body.variables)]
                                                  (tset variables variable.name variable.value)))
                                              variables))
                                          inspect
                                          (stringx.splitlines))
                          :breakpoint-details (->> (accumulate [breakpoint-details {}
                                                                _ event (pairs tui.events)]
                                                     (let [content (?. event :content :content)]
                                                       (match [(?. content :type)
                                                               (?. content :event)]
                                                         [:event :stopped]
                                                         (do
                                                           (tset breakpoint-details :reason content.body.reason)
                                                           (tset breakpoint-details :description content.body.description)
                                                           (tset breakpoint-details :text content.body.text)))
                                                       breakpoint-details))
                                                   inspect
                                                   (stringx.splitlines))
                          :stack-trace (accumulate [stack-trace []
                                                    _ event (pairs tui.events)]
                                         (let [content (?. event :content :content)]
                                           (match [(?. content :type)
                                                   (?. content :command)]
                                             [:response :stackTrace]
                                             (each [_ frame (ipairs content.body.stackFrames)]
                                               (table.insert stack-trace
                                                             (.. frame.source.path ":" frame.line
                                                                 " - " frame.name))))
                                           stack-trace))
                          _ "")
          content (->> (icollect [line-no line (ipairs content-lines)]
                         (let [cursor-seq (t.cursor.position.set_seq
                                            (- (+ line-no content-location.row) 1)
                                            content-location.column)
                               truncated-line (string.sub line 0 content-size.width)]
                           (.. cursor-seq truncated-line)))
                       (stringx.join ""))]
      (.. (t.cursor.position.set_seq content-location.row content-location.column)
          content)))

  (fn window-border-term-seq [component-id focused? clear-content?]
    (let [plan (. tui.location-plan component-id)]
      (when plan
        (let [component (. tui.components component-id)
              title (.. (if component.key (.. component.key ": ") "") component.title)
              location plan.location
              size plan.size]
          (.. (t.cursor.position.set_seq location.row location.column)
              (if focused?
                (.. (t.text.stack.push_seq {:fg "yellow"})
                    (t.draw.box_seq size.height size.width my-focused-box-fmt clear-content? title)
                    (t.text.stack.pop_seq))
                (t.draw.box_seq size.height size.width my-box-fmt clear-content? title)))))))

  (fn window-term-seq [component-id]
    (let [plan (. tui.location-plan component-id)]
      (when plan
        (.. (window-border-term-seq component-id (= component-id tui.active.window) true)
            (window-content-term-seq component-id plan.location plan.size)))))

  (fn make-location-plan [layout size]
    (local location-plan {})

    (fn traverse [component-id component-layout location size]
      (match component-layout.type
        :container
        (let [(next content) (pairs component-layout.content)
              (subcomponent-id subcomponent-layout) (next content)]
          (traverse subcomponent-id subcomponent-layout location size))

        :horizontal-split
        (let [sizes (split-real-sizes component-layout size.width)]
          (accumulate [location2 location
                       _ subcomponent-id (ipairs component-layout.order)]
            (let [size2 {:width (. sizes subcomponent-id)
                         :height size.height}
                  next-location {:row location2.row
                                 :column (+ location.column size2.width)}]
              (traverse subcomponent-id (. component-layout.content subcomponent-id) location2 size2)
              next-location)))

        :vertical-split
        (let [sizes (split-real-sizes component-layout size.height)]
          (accumulate [location2 location
                       _ subcomponent-id (ipairs component-layout.order)]
            (let [size2 {:width size.width
                         :height (. sizes subcomponent-id)}
                  next-location {:row (+ location2.row size2.height)
                                 :column location.column}]
              (traverse subcomponent-id (. component-layout.content subcomponent-id) location2 size2)
              next-location)))

        _ (set (. location-plan component-id) {:location location :size size})))

    (traverse nil layout {:row 1 :column 1} tui.size)
    location-plan)

  (fn tui.initialize []
    (t.initialize {:displaybackup true :filehandle io.stdout})
    (t.cursor.visible.set false)
    (set tui.initialized true))

  (fn tui.shutdown []
    (t.shutdown))

  (fn redraw-window-border [component-id focused?]
    ;; TODO: maybe this is too much work for too little sugar
    (let [term-seq (window-border-term-seq component-id focused? false)]
      (write term-seq)))

  (fn redraw-component [component-id]
    (let [window-seq (window-term-seq component-id)]
      (write window-seq)))

  (fn tui.redraw []
    (when (not tui.initialized)
      (tui.initialize))

    (set tui.size (usable-termsize))
    (set tui.location-plan (make-location-plan (. tui.layouts tui.active.screen) tui.size))

    (t.clear.screen)
    (each [component-id _ (pairs tui.location-plan)]
      (let [window-seq (window-term-seq component-id)]
        (write window-seq))))

  (fn tui.handle-command [command params]
    (match command
      :add-event (do
                   (when params.content.content-raw
                     (set params.content.content-formatted (format-with-jq params.content.content-raw)))
                   (table.insert tui.events params)
                   (let [first-event? (= 1 (length tui.events))]
                     (when first-event?
                       (set tui.active.event-list--event 1)
                       (set params.selected true)
                       (redraw-component :event-details)))
                   (redraw-component :variables)
                   (redraw-component :stack-trace)
                   (redraw-component :breakpoint-details)
                   (redraw-component :event-list))
      :move-cursor (match tui.active.window
                    :event-list
                    (match params.direction
                      :down (let [previously-selected tui.active.event-list--event]
                              (when (and previously-selected (< previously-selected (length tui.events)))
                                (let [newly-selected (+ 1 previously-selected)]
                                  (set tui.active.event-list--event newly-selected)
                                  (set (. tui.events newly-selected :selected) true)
                                  (set (. tui.events previously-selected :selected) false))
                                (redraw-component :event-list)
                                (redraw-component :event-details)))
                      :up (let [previously-selected tui.active.event-list--event]
                            (when (and previously-selected (< 1 previously-selected))
                              (let [newly-selected (- previously-selected 1)]
                                (set tui.active.event-list--event newly-selected)
                                (set (. tui.events newly-selected :selected) true)
                                (set (. tui.events previously-selected :selected) false))
                              (redraw-component :event-list)
                              (redraw-component :event-details)))))
      :select-window (let [currently-active tui.active.window
                           new-active (tablex.find_if tui.components
                                                      (fn [component]
                                                        (= component.key params.window-key)))]
                       (when (and new-active (not= currently-active new-active))
                         (set tui.active.window new-active)
                         (redraw-window-border currently-active false)
                         (redraw-window-border new-active true)))
     :select-screen (when (. tui.layouts params.screen-id)
                      (set tui.active.screen params.screen-id)
                      (tui.redraw))))

  (fn tui.should-resize? []
    (not (tablex.deepcompare (usable-termsize) tui.size)))

  tui)

{:make-tui make-tui}
