(local cjson (require "cjson.safe"))
(local socket (require "socket"))

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
     :content-data content-data
     :content content-json}))

{:write-message write-message
 :make-request make-request
 :read-message read-message}
