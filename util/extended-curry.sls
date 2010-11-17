#!r6rs

(library (mpl util extended-curry)

 (export curry)

 (import (rnrs)
         (for (mpl util symbols) (meta 1)))

 (define-syntax curry-helper

   (syntax-rules ()

     ( (curry-helper (original ...) procedure parameter rest ...)

       (lambda (parameter)

         (curry-helper (original ...) procedure rest ...)) )

     ( (curry-helper (original ...) procedure)

       (procedure original ...) )))

 (define-syntax curry

   (lambda (x)

     (syntax-case x ()

       ( (curry procedure parameter ...)

         (with-syntax ( ((sorted ...)

                         (datum->syntax
                          (syntax procedure)
                          (list-sort symbol<?
                                     (syntax->datum
                                      (syntax (parameter ...)))))) )

                      (syntax

                       (curry-helper (parameter ...) ;; original
                                     procedure
                                     sorted ...))) ))))
 )