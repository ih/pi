(import (r6rs-libraries gui pstk))

(tk-start)

(let* ((label (tk 'create-widget 'label
                  'text: "Hello, World!"
                  'foreground: 'red))
       (quit-button (tk 'create-widget 'button
                        'text: "Goodbye"
                        'command: tk-end)))
  (tk/pack label quit-button)
  (tk-event-loop))


(define f (tk 'create-widget 'frame))

(define b (f 'create-widget 'button))