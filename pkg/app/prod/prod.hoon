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
        ;style:"{(trip style)}"
        ;meta
          =name     "viewport"
          =charset  "utf-8"
          =context  "width=device-width, initial-scale=1";
      ==
    ::
      ;body
        ;div(class "container")
          ;div(class "response-container")
            ;+  ?~  msgs  :/""
                ?:  gud.u.msgs
                  ;div#status.green:"{(trip txt.u.msgs)}"
                ;div#status.red:"{(trip txt.u.msgs)}"
          ==
        ::
          ;div(class "instructions-container")
            ;p:"Attach some message in a chat, and write \"!remind-me <some @dr>\""
            ;p:"E.g. \"!remind-me ~d20\" and an attachment would produce a reminder in 20 days"
          ==
        ::
          ;div(class "kick-container")
            ;form(method "post")
              ;input(name "act", value "kick", style "display: none;");
              ;button:"Kick Me 🦶"
            ==
          ==
        ::
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
                ;table
                  ;tr
                    ;th:"delete"
                    ;th:"due date"
                    ;th:"link"
                  ==
                ::
                  ;*  (minds:make (~(got by tac) u.flg))
                ==
          ==
        ==
      ==
    ==
  ++  style
    '''
    * { margin: 0.2em; padding: 0.2em; font-amily: monospace; }
    .red {
      color: red;
    }
    .green {
      color: green;
    }
    '''
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
            ;p:"see reminders"
          ==
        ==
      ==
    ++  minds
      |=  doz=(set (pair @da (pair ship time)))
      ^-  marl
      =+  hos=(scow %p p:(need flg))
      =+  cha=(scow %tas q:(need flg))
      =/  next
        |=([a=[@da *] b=[@da *]] (gth -.a -.b))
      %+  turn  (sort ~(tap in doz) next)
      |=  (pair @da (pair ship time))
      ^-  manx
      ;tr
        ;td
          ;form(method "post")
            ;input(name "act", value "drop", style "display: none;");
            ;input(name "host", value "{hos}", style "display: none;");
            ;input(name "chat", value "{cha}", style "display: none;");
            ;input(name "time", value "{(scow %da p)}", style "display:none;");
            ;button:"Remove Reminder 🚫"
          ==
        ==
        ;td:"{(scow %da p)}"
        ;td:"/1/chan/chat/{hos}/{cha}/msg/{(scow %p p.q)}/{(scow %ud q.q)}"
      ==
    --
  --
--
