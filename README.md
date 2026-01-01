# ParenWorks Systems Website

A modern website for ParenWorks Systems built with Common Lisp using the [Radiance](https://shirakumo.org/docs/radiance) web application framework.

## About ParenWorks Systems

ParenWorks Systems is a systems development company that specialises in using Common Lisp to find solutions to problems and innovating in Common Lisp.

## Prerequisites

- A Common Lisp implementation (SBCL recommended)
- [Quicklisp](https://www.quicklisp.org/)
- Radiance framework

## Installation

### 1. Install the Shirakumo dist (if not already installed)

```lisp
(ql-dist:install-dist "http://dist.shirakumo.org/shirakumo.txt")
```

### 2. Install Radiance

```lisp
(ql:quickload :radiance)
```

### 3. Configure the Server Implementation

By default, Radiance uses Hunchentoot. To use a different server (e.g., Woo), you can configure the environment.

For **Woo** server (recommended for production):
```lisp
;; In your Radiance configuration
(setf (mconfig :radiance-core :interfaces :server) "r-woo")
```

For **Clack** (flexible backend):
```lisp
(setf (mconfig :radiance-core :interfaces :server) "r-clack")
```

### 4. Load the ParenWorks module

Make sure this directory is in a location ASDF can find (e.g., `~/common-lisp/` or Quicklisp's `local-projects`):

```lisp
(ql:register-local-projects)
(ql:quickload :parenworks)
```

## Running the Website

### Start Radiance

```lisp
(radiance:startup)
```

The website will be available at: `http://localhost:8080/`

### Stop Radiance

```lisp
(radiance:shutdown)
```

## Project Structure

```
parenworks/
├── parenworks.asd      # ASDF system definition
├── module.lisp         # Radiance module definition
├── pages.lisp          # Page definitions and routing
├── static/
│   └── parenworks.css  # Stylesheet
└── template/
    ├── home.ctml       # Home page template
    ├── about.ctml      # About page template
    ├── services.ctml   # Services page template
    └── contact.ctml    # Contact page template
```

## Pages

- **Home** (`/`) - Landing page with hero section and feature highlights
- **About** (`/about`) - Company information and philosophy
- **Services** (`/services`) - Detailed service offerings
- **Contact** (`/contact`) - Contact form and information

## Customisation

### Styling

Edit `static/parenworks.css` to modify the visual appearance. The site uses CSS custom properties (variables) for easy theming:

```css
:root {
  --color-primary: #8b5cf6;
  --color-accent: #22d3ee;
  /* ... */
}
```

### Templates

Templates use [Clip](https://shinmera.github.io/clip) templating with XHTML5. Edit files in the `template/` directory.

### Adding New Pages

1. Add a new template file in `template/`
2. Define a new page in `pages.lisp`:

```lisp
(define-page my-page "parenworks/my-page" (:clip "my-page.ctml")
  (r-clip:process T))
```

## Server Options

Radiance supports multiple server backends through its interface system:

| Server | Package | Notes |
|--------|---------|-------|
| Hunchentoot | `r-simple-server` | Default, pure CL |
| Woo | `r-woo` | High performance, requires libev |
| Clack | `r-clack` | Flexible, multiple backends |

To change the server, modify the interface mapping in your Radiance environment configuration.

## Development Tips

### Enable Debugger

```lisp
(setf radiance:*debugger* T)
```

### Reload Templates

Templates are cached. To reload after changes:

```lisp
(asdf:load-system :parenworks :force t)
```

### Interactive Development

Use SLIME/SLY for REPL-driven development. Changes to page definitions take effect immediately after recompilation.

## License

Copyright © 2026 ParenWorks Systems. All rights reserved.

## Contact

- Website: [parenworks.systems](https://parenworks.systems)
- Email: hello@parenworks.systems
