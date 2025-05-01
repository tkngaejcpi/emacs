(straight-use-package 'gptel)
(straight-use-package 'dash)

(require 'dash)

;; bind keys
(keymap-set global-map
	    "<f5>"
	    #'gptel-menu)

;; setup backend and models
(setq gptel-backend
      (gptel-make-openai "OpenRouter"
	:host "openrouter.ai"
	:endpoint "/api/v1/chat/completions"
	:stream t
	:key #'gptel-api-key-from-auth-source
	:models '((openai/o4-mini-high
		   :description "Fast, effective reasoning with efficient performance in coding and visual tasks, with reasoning_effort set to high."
		   :capabilities (reasoning media tool-use json url)
		   :mime-types ("image/jpeg" "image/png" "image/gif" "image/webp")
		   :context-window 200
		   :input-cost 1.10
		   :output-cost 4.40
		   :cutoff-date "2024-05")
		  (google/gemini-2.5-pro-preview-03-25
		   :description "Enhanced reasoning, multimodal understanding & advanced coding"
		   :capabilities (tool-use json media)
		   :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
				"application/pdf" "text/plain" "text/csv" "text/html")
		   :context-window 1000
		   :input-cost 1.25
		   :output-cost 10.00
		   :cutoff-date "2025-01")
		  (google/gemini-2.5-flash-preview
		   :description "Best Gemini model in terms of price-performance, offering well-rounded capabilities"
		   :capabilities (tool-use json media)
		   :mime-types ("image/png" "image/jpeg" "image/webp" "image/heic" "image/heif"
				"application/pdf" "text/plain" "text/csv" "text/html")
		   :context-window 1000
		   :input-cost 0.15
		   :output-cost 0.60
		   :cutoff-date "2025-01"))))

;; tools
(with-eval-after-load 'gptel
  (gptel-make-tool
   :category "emacs"
   :name "read_documentation"
   :description "Read documentation of a given function or variable in Emacs, and return a string which is either the documentation or a message."

   :function (lambda (symbol-id)
	       (let ((symbol (intern symbol-id)))
		 (cond ((fboundp symbol)
			(-> symbol
			    (describe-function)
			    (save-window-excursion)
			    (substring-no-properties)))

		       ((boundp symbol)
			(-> symbol
			    (describe-function)
			    (save-window-excursion)
			    (substring-no-properties)))

		       (t "No documentation."))))

   :args '((:name "symbol-id" :type string :description "The identifier (name) of the function or variable you want."))))

(provide 'init-gptel)
