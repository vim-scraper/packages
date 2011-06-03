" Vim filetype plugin
" Language:	LaTeX
" Maintainer: Srinath Avadhanula
" Last Change:	Mon Sep 24 13:04:04  2001

" Only do this when not done yet for this buffer
if exists("b:did_ftplugin")
  finish
endif

" Don't load another plugin for this buffer
let b:did_ftplugin = 1

exec "setlocal isk=".&isk.",:"

imap <buffer> BAR \left
imap <buffer> BEN \begin{enumerate}
imap <buffer> BIT \begin{itemize}
imap <buffer> BEQ \begin{equation}
imap <buffer> BQN \begin{eqnarray}
imap <buffer> BFG \begin{figure}[h]
imap <buffer> BFE \begin{figure}
imap <buffer> BTB \begin{tabular}{}
imap <buffer> BTA \begin{table}

" picture
" \begin{picture}(4,4)
" \put(0.5,0.0){\framebox(4,4){_}}
" \end{picture}
imap <buffer> PIC \begin{picture}(4,4)

imap <buffer> MAT \left[
imap <buffer> FRAC \frac{}{}hhi
inoremap <buffer> _ _{}i
inoremap <buffer> ^ ^{}i
inoremap <buffer> DOT \dot{}i
inoremap <buffer> DDOT \ddot{}i
inoremap <buffer> $$ $$i
inoremap <buffer> == &=&

imap <buffer> \a<tab> \alpha
imap <buffer> \b<tab> \beta
imap <buffer> \c<tab> \chi
imap <buffer> \d<tab> \delta
imap <buffer> \e<tab> \epsilon
imap <buffer> \f<tab> \phi
imap <buffer> \g<tab> \gamma
imap <buffer> \h<tab> \eta
imap <buffer> \k<tab> \kappa
imap <buffer> \l<tab> \lambda
imap <buffer> \m<tab> \mu
imap <buffer> \n<tab> \nu
imap <buffer> \p<tab> \pi
imap <buffer> \q<tab> \theta
imap <buffer> \r<tab> \rho
imap <buffer> \s<tab> \sigma
imap <buffer> \t<tab> \tau
imap <buffer> \u<tab> \upsilon
imap <buffer> \v<tab> \varsigma
imap <buffer> \w<tab> \omega
imap <buffer> \x<tab> \xi
imap <buffer> \y<tab> \psi
imap <buffer> \z<tab> \zeta
