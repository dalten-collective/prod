/-  *prod
/+  r=rudder
::
^-  (page:r tack poke)
|_  [bol=bowl:gall odo=order:r tac=tack]
++  final  (alert:r 'prod' build)
::
++  argue
  |=  [head=header-list:http body=(unit octs)]
  ?>  authenticated.odo
  ^-  $@(brief:r poke)
  =/  args=(map @t @t)
    ?~(body ~ (frisk:r q.u.body))
  ?.  (~(has by args) 'act')  'unsatisfactory poke'
  ?:  ?=(%kick (~(got by args) 'act'))  [%kick ~]
  ?.  ?=(%drop (~(got by args) 'act'))  'unsatisfactory poke'
  =+  hos=`@p`(slav %p (~(got by args) 'host'))
  =+  cha=`@tas`(slav %tas (~(got by args) 'chat'))
  =+  wen=`@da`(slav %da (~(got by args) 'time'))
  [%drop [hos cha] wen]
::
++  build
  |=  $:  args=(list [k=@t v=@t])
          msgs=(unit [gud=? txt=@])
      ==
  ^-  reply:r
  =/  flg=(unit flag)
    ?~  hav=(~(get by (malt args)) 'flag')  ~
    %+  rush  u.hav
    ;~((glue cab) ;~(pfix sig fed:ag) sym)
  |^  [%page page]
  ++  page
    ^-  manx
    ;html
      ;head
        ;title:"%prod (not a test)"
        ;meta
          =name     "viewport"
          =charset  "utf-8"
          =context  "width=device-width, initial-scale=1";
      ==
    ::
      ;body
        ;div(class "container")
          ;div(class "kick-container")
            ;form(method "post")
              ;input(name "act", value "kick", style "display: none;");
              ;button:"Kick Me 🦶"
            ==
          ==
          ;div(class "reminder-container")
            ;+  ?~   flg
                  ;table
                    ;tr
                      ;th:"host"
                      ;th:"chat"
                      ;th:"link"
                    ==
                  ::
                    ;*  (chats:make ~(key by tac))
                  ==
                ;p:"fail"
          ==
        ==
      ==
    ==
  ++  make
    |%
    ++  chats
      |=  dem=(set flag)
      ^-  marl
      %+  turn  ~(tap in dem)
      |=  (pair ship term)
      ^-  manx
      ;tr
        ;td:"{(scow %p p)}"
        ;td:"{(scow %tas q)}"
        ;td
          ;a(href "./prod?flag={(scow %p p)}_{(scow %tas q)}")
            ;p:"test"
          ==
        ==
      ==
    --
  --
--
