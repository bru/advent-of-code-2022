# 🎄 Advent of Code 2022

My take at the [AoC2022](https://adventofcode.com), using [clojure](https://clojure.org) and [Clerk](https://clerk.vision).

I used the excellent [Advent of Clerk](https://github.com/nextjournal/advent-of-clerk) template as a starter; you should check it out if you
want to participate, or you can tag along here for as long as I'll manage to stick to the task.

If you want to enjoy the Clerk notebooks, they're published in the [Clerk Garden](https://github.clerk.garden/bru/advent-of-code-2022).

## Usage

Fork & clone this repo, make sure you have [Clojure
installed](https://clojure.org/guides/install_clojure), then run:

``` shell
clj -M:nextjournal/clerk nextjournal.clerk/serve! --watch-paths src --port 7878 --browse
```

This will start the Clerk webserver on port 7878 and watch the `src/`
directory for changes and open Clerk in your browser.

Alternatively, follow the instructions in the [book of Clerk](https://book.clerk.vision/#🔪_editor_integration) to integrate with your editor of choice.

In the case of Emacs, add the following to your config:

``` emacs-lisp
(defun clerk-show ()
  (interactive)
  (when-let
      ((filename
        (buffer-file-name)))
    (save-buffer)
    (cider-interactive-eval
     (concat "(nextjournal.clerk/show! \"" filename "\")"))))

(define-key clojure-mode-map (kbd "<M-return>") 'clerk-show)
```

and then use `M-return` to update the clerk view in the browser.

**NOTE:** remember to connect the repl and start the `clerk` server though.
To do that, you can eval the following code (you can find it in `user.clj`):

``` clojure
(clerk/serve! {:port 7878 :browse true})
```

This will also have the effect of opening the clerk page in your default browser.

## Personal notes

This is supposed to be a cosy pastime in the days leading to the winter holidays.

In that spirit, I'm exercising my right to fidget, explore ideas, exercise parts
of the standard library and techniques I'd like to dust off, and venture (or not)
into long winded commentary about my train of thoughts.

in general I'm **not expecting my solutions to be regarded as best practices or even good examples
of idiomatic clojure**.

If you're looking for best in class, keep looking: ultimately here we're just **having fun**.
