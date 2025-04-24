## `eglot-dispatcher`

**A dynamic, project-aware LSP dispatcher for [Eglot](https://github.com/joaotavora/eglot)**, designed to support multiple language server configurations for the *same major mode* — including simultaneous LSPs via [LSPX](https://github.com/thefrontside/lspx).

---

### 🧠 Motivation

Eglot is a lightweight and elegant client for Language Server Protocol (LSP), but it assumes a one-to-one mapping between a major mode and a single language server. In practice, however, real-world projects often require more flexibility:

- Different **LSPs for the same language** (e.g., TypeScript projects that use Angular, React, or no framework at all)
- Running **multiple LSPs simultaneously** for the same buffer (e.g., a language server plus a linter)
- Project-specific configurations that vary based on files like `angular.json`, `package.json`, or `vite.config.ts`

`eglot-dispatcher` solves this by letting you define a **declarative list of strategies** that determine which LSP(s) to launch based on file type and project context — all without modifying `eglot` itself.

---

### ✨ Features

- 🔄 Dynamically chooses the correct LSP for a file based on project structure
- 🧠 Supports multiple major modes per strategy
- 🔁 Automatically runs **multiple LSPs per buffer** via [lspx](https://github.com/thefrontside/lspx)
- 🔍 Supports `:initializationOptions`, `:command`, and other extended configuration options
- 💡 Clean, declarative interface designed for scalability and reuse

---

### 📦 Installation

Clone the repo into your `load-path`:

```sh
git clone https://github.com/Askath/eglot-dispatcher ~/emacs.d/lisp/eglot-dispatcher
```

Then in your Emacs configuration:

```elisp
(add-to-list 'load-path "~/emacs.d/lisp/eglot-dispatcher")
(require 'eglot-dispatcher)
```

---

### 🛠️ Usage

#### Hook it into relevant modes

```elisp
(add-hook 'typescript-ts-mode-hook #'eglot-dispatcher-dispatch)
(add-hook 'html-ts-mode-hook #'eglot-dispatcher-dispatch)
```

#### Define your LSP strategies

```elisp
(setq eglot-strategies-list
	  '((:name "Angular + ESLint"
		 :modes (typescript-ts-mode html-ts-mode)
		 :project-root "angular.json"
		 :command (lambda (root)
					(list
					 `("npx" "ngserver"
					   "--stdio"
					   "--tsProbeLocations" ,(expand-file-name "node_modules" root)
					   "--ngProbeLocations" ,(expand-file-name "node_modules" root))
					 '("vscode-eslint-language-server" "--stdio"))))

		(:name "React"
		 :modes (typescript-ts-mode)
		 :project-root "package.json"
		 :predicate (lambda (root)
					  (file-exists-p (expand-file-name "node_modules/react" root)))
		 :command (lambda (_root)
					'("typescript-language-server" "--stdio")))

		(:name "Default TypeScript"
		 :modes (typescript-ts-mode)
		 :command (lambda (_root)
					'("typescript-language-server" "--stdio")))))
```

---

### 📋 Strategy Fields

| Field | Description |
|-------|-------------|
| `:name` | Strategy name (used for logging/debugging) |
| `:modes` | List of major modes this strategy applies to |
| `:project-root` | A filename to search upward for to detect the project root |
| `:predicate` *(optional)* | A function `(fn root)` that returns non-nil if the strategy should be used |
| `:command` | A function `(fn root)` that returns:<ul><li>A single command list (e.g. `("tsserver" "--stdio")`)</li><li>A plist with `:command`, `:initializationOptions`, etc.</li><li>A list of command lists → triggers `lspx`</li></ul> |

---

### 🔁 Multiplexing with `lspx`

If your command function returns **multiple command lists**, `eglot-dispatcher` automatically invokes [lspx](https://github.com/thefrontside/lspx) to run them in parallel:

```elisp
:command (lambda (_root)
		   (list
			'("angular-language-server" "--stdio")
			'("eslint-language-server" "--stdio")))
```

This becomes:

```elisp
("lspx" "--lsp" "angular-language-server --stdio"
		"--lsp" "eslint-language-server --stdio")
```

---

### ⚙️ Initialization Options Example

You can specify Eglot-compatible initialization options like so:

```elisp
:command (lambda (_root)
		   '(:command ("typescript-language-server" "--stdio")
			 :initializationOptions (:tsserver (:logVerbosity "verbose"))))
```

---

### ✅ Use Cases

- Use `angular-language-server` + `eslint-language-server` in Angular projects
- Use `typescript-language-server` in React projects with framework-specific flags
- Fall back to the default TypeScript server in generic projects
- Extend to Vue, Python, Go, or any other Eglot-supported language

---

### 📄 License

MIT

---

### 🤝 Contributing

Feedback and contributions are very welcome. Please feel free to:

- Report issues
- Suggest new strategies or use cases
- Contribute improvements or enhancements
