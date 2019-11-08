# Acknowledgements
Thanks to _aculich_ for [his configs](https://github.com/aculich/.emacs.d).
It's very difficult to find any _sane_ configurations in the Emacs world.

# TODO
- [ ] Compile a list of "easy" shortcuts (C-whatever, M-whatever) that I think are absolute *garbage*
- [ ] Comment/uncomment line/REGION with C-/ (I think I will be slowly heading towards VSCode controls...)
- [ ] Text manipulation around sentences is useless for programming -> change definition of a sentence to one revolving around parens
- [ ] which-func doesn't really work well
- [ ] C-BACKSPACE deletes too much; just stop at the beginning of the line, please

# Done
- [x] Marking a region and pressing Backspace should delete it
- [x] Paredit is trying to be too smart and doesn't allow to delete _unmatched_ left paren, when it was left there by accident.  
- [x] Golden ratio for buffers is broken again
- [x] Copy and pasting in Emacs is completely backwards; 'yank' is used in wrong context and it bothers no-one?
- [x] S-DEL for deleting whole line

# Changed my mind
- [ ] C-\<left/right-arrow\> moves always too far; it should stop before brackets and line beginnings  
  I'll stick to slurping and barfing with those keys
- [ ] Moving sexps around with C-\<arrows\> is a terrible, terrible idea for my muscle memory -> just move word by word
- [ ] Vim's % to move to a closing paren ([solution](https://www.emacswiki.org/emacs/NavigatingParentheses))

# WTF commands
- C-z -> absoulte winner
- C-k -> recenter-top-bottom
