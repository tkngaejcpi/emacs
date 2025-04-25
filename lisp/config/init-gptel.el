(straight-use-package 'gptel)

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

(provide 'init-gptel)
