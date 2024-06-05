# `read-duration`

This package provides a function that reads a human-readable time duration from
a user and returns the value in seconds, minutes, hours or any other time unit
defined in the `read-duration-return-units` custom variable. The function shows
available input characters (available time unit multipliers (see
`read-duration-multipliers`) and digits) in the prompt and checks the input
while the user is typing. If the user mistypes, the function calls the `ding'
function.

## Usage

``` elisp
(read-duration "Duration:")
```

## Installation

``` elisp
(unless (package-installed-p 'read-duration)
  (package-vc-install
   '(read-duration
     :vc-backend Git
     :url "https://github.com/ichernyshovvv/read-duration.el"
     :branch "master")))
```
