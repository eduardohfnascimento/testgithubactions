REPORT zfoobar.
 WRITE 'Hello World'.

DATA moo TYPE i VALUE 2.
WRITE moo.
moo = 4.

LOOP AT lt_foo ASSIGNING FIELD-SYMBOL(<ls_foo>).
  WRITE 'bar'.
ENDLOOP.

FORM foo.
  DATA boo TYPE i.
ENDFORM.