# microblog.el

A high-performance, offline-first Emacs client for [Micro.blog](https://micro.blog). 

Inspired by native desktop blogging apps, `microblog.el` stores your entire post history locally using Emacs 29's built-in SQLite. This allows for instant loading, full-text offline search, and a snappy interface. It utilizes `curl` under the hood to bypass Emacs's internal network quirks, guaranteeing flawless UTF-8 emoji support and rock-solid media uploads.

## Dependencies

* **Emacs 29.1+** (Requires built-in SQLite support).
* **curl** (For API communication and media uploads).
* **pandoc** *(Optional but highly recommended)*: Automatically converts downloaded HTML posts into clean Markdown for editing.
* **markdown-mode** *(Optional)*: Provides syntax highlighting in the compose window.

## Installation

1. Download `microblog.el` and place it in your Emacs `load-path` (e.g., `~/.emacs.d/lisp/`).
2. Add the following to your `init.el`:

```elisp
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'microblog)
```

;; Map a global shortcut to open the client
(global-set-key (kbd "C-c m") 'microblog-list)

## Authentication (Required)

To talk to the API, Emacs needs your Micro.blog App Token. We use Emacs's built-in, secure `auth-sources` system so your token is never hardcoded in your configuration files.

1. Generate an App Token on the Micro.blog website under **Account -> App tokens**.
2. Open (or create) your `~/.authinfo` or `~/.authinfo.gpg` file.
3. Add the following line:

```text
machine micro.blog login apikey password YOUR_APP_TOKEN_HERE
```
*(Replace `YOUR_APP_TOKEN_HERE` with your actual token).*

## Usage

Start the app by running `M-x microblog-list` (or using your custom keybinding).

If your list is empty, run `M-x microblog-sync-posts` to download your timeline into the local SQLite database.

### List View Keybindings

When you are in the `*Micro.blog Headers*` buffer, use the following keys:

* `c` - **Compose** a new post.
* `e` - **Edit** the currently highlighted post.
* `w` - **Copy URL** of the currently highlighted post to your system clipboard.
* `s` - **Search** the full text of all posts, titles, and tags.
* `g` - **Refresh** the view (and clear any active search filters).

### Edit & Compose View Keybindings

When writing or editing a post, Emacs will present an email-style buffer with `Title:` and `Categories:` headers. It automatically enables word-wrapping (`visual-line-mode`) for a comfortable writing experience.

* `C-c C-c` - **Publish / Save**. Parses your buffer and sends it to Micro.blog.
* `C-c C-t` - **Tags / Categories**. Opens an auto-complete prompt at the bottom of the screen containing all tags you've previously used in your database. You can select multiple tags separated by commas.
* `C-c C-a` - **Attach Image**. Prompts you to select a local image file and provide Alt Text. Uploads the image to Micro.blog in the background and inserts the Markdown `![alt](url)` link directly at your cursor.
* `C-c C-k` - **Cancel** (Kills the buffer without saving).

## Managing the Database

* `M-x microblog-sync-posts`: Force a manual download of your latest posts from the server.
* `M-x microblog-reset-db`: Wipes the local SQLite database entirely. Use this if you want to perform a clean, fresh sync from the server.
