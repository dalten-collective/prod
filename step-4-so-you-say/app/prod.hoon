/-  c=chat
/+  verb, dbug, default-agent
::
|%
::
+$  versioned-state  $%(state-0)
::
+$  state-0  [%0 ~]
::
::
::  boilerplate
::
+$  card  card:agent:gall
--
::
%+  verb  &
%-  agent:dbug
=|  state-0
=*  state  -
::
^-  agent:gall
::
=<
  |_  =bowl:gall
  +*  this  .
      def  ~(. (default-agent this %|) bowl)
      eng   ~(. +> [bowl ~])
  ++  on-init
    ^-  (quip card _this)
    ~>  %bout.[0 '%prod +on-init']
    =^  cards  state
      abet:init:eng
    [cards this]
  ::
  ++  on-save
    ^-  vase
    ~>  %bout.[0 '%prod +on-save']
    !>(state)
  ::
  ++  on-load
    |=  ole=vase
    ~>  %bout.[0 '%prod +on-load']
    ^-  (quip card _this)
    =^  cards  state
      abet:(load:eng ole)
    [cards this]
  ::
  ++  on-poke
    |=  [mar=mark vaz=vase]
    ~>  %bout.[0 '%prod +on-poke']
    ^-  (quip card _this)
    `this
  ::
  ++  on-peek
    |=  =path
    ~>  %bout.[0 '%prod +on-peek']
    ^-  (unit (unit cage))
    [~ ~]
  ::
  ++  on-agent
    |=  [wir=wire sig=sign:agent:gall]
    ~>  %bout.[0 '%prod +on-agent']
    ^-  (quip card _this)
    =^  cards  state
      abet:(dude:eng wir sig)
    [cards this]
  ::
  ++  on-arvo
    |=  [wir=wire sig=sign-arvo]
    ~>  %bout.[0 '%prod +on-arvo']
    ^-  (quip card _this)
    `this
  ::
  ++  on-watch
  |=  =path
  ~>  %bout.[0 '%prod +on-watch']
  ^-  (quip card _this)
  `this
  ::
  ++  on-fail
    ~>  %bout.[0 '%prod +on-fail']
    on-fail:def
  ::
  ++  on-leave
    ~>  %bout.[0 '%prod +on-init']
    on-leave:def
  --
|_  [bol=bowl:gall dek=(list card)]
+*  dat  .
++  emit  |=(=card dat(dek [card dek]))
++  emil  |=(lac=(list card) dat(dek (welp lac dek)))
++  abet
  ^-  (quip card _state)
  [(flop dek) state]
::  +hear: nothing has changed
::
++  hear
  %-  emit
  [%pass /chats %agent [our.bol %chat] %watch /briefs]
::  +on-init: nothing has changed
::
++  init
  ^+  dat
  hear
::  +on-load: nothing has changed
::
++  load
  |=  vaz=vase
  ^+  dat
  ?>  ?=([%0 *] q.vaz)
  dat(state !<(state-0 vaz))
::  +dude: nothing has changed
::
++  dude
  |=  [pol=(pole knot) sig=sign:agent:gall]
  ^+  dat
  ?+    pol  ~|(bad-dude-wire/pol !!)
      [%chats ~]
    ?+  -.sig  dat
      %kick       hear
      %watch-ack  %.(dat ?~(p.sig same (slog u.p.sig)))
    ::
        %fact
      ?:  ?=(%chat-leave p.cage.sig)
        dat  :: we're going to want to delete reminders later
      ?.  ?=(%chat-brief-update p.cage.sig)  dat
      ::  see what we got from chat
      =/  upd  !<([whom:c brief:briefs:c] q.cage.sig)
      ::  raw output
      ~&  >>>  upd  dat
      ::  scry output
      :: ?.  ?=(%flag -.-.upd)  dat
      ::   ~&  >
      ::     .^  (map @da writ:c)
      ::       %gx
      ::       ;:  welp
      ::         /(scot %p our.bol)/chat/(scot %da now.bol)/chat
      ::         /(scot %p p.p.-.upd)/[q.p.-.upd]/writs/newest/1/noun
      ::       ==
      ::     ==
      :: dat
      ::
    ==
  ==
--