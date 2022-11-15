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
::  +dude: handles on-agent (agents are dudes)
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
      :: ~&  >>>  upd  dat
      ::  scry output
      ?.  ?=(%flag -.-.upd)  dat
        ~&  >
          .^  (map @da writ:c)
            %gx
            ;:  welp
              /(scot %p our.bol)/chat/(scot %da now.bol)/chat
              /(scot %p p.p.-.upd)/[q.p.-.upd]/writs/newest/1/noun
            ==
          ==
      dat
      ::  engine use
      :: =^  cards  state
      ::   bl-abet:bl-behn:bl-test:bl-last:(bl-prep:bl upd)
      :: (emil cards)
      ::
    ==
  ==
::  +bl: a blab engine
::
++  bl
  |_  $:  fug=(unit flag)
          ref=(unit (pair ship @da))
          con=(unit content:c)
          wen=(unit @da)
          caz=(list card)
      ==
  +*  blab  .
      our  (scot %p our.bol)
      now  (scot %da now.bol)
      bla  /[our]/chat/[now]/chat
  ++  bl-emit  |=(c=card blab(caz [c caz]))
  ++  bl-abet  [(flop caz) state]
  ++  bl-prep
    ::  we'll check if this is a flag,
    ::  if it is we'll add it as a fug
    blab
    :: |=[whom:c *]
    :: ?.(?=(%flag -.w) blab blab(fug `p.w))
  ++  bl-behn
    ::  we'll set a behn timer on some
    ::  wire that arvo can reuse later
    blab
    :: ?~  wen  blab
    :: =+  flg=(need fug)
    :: =+  ref=(need ref)
    :: =;  wir=path
    ::   %.  [%pass wir %arvo %b [%wait u.wen]]
    ::   bl-emit(about (~(put ju about) flg [u.wen ref]))
    :: ;:  weld  
    ::   /when/(scot %da u.wen)
    ::   /flag/(scot %p p.flg)/[q.flg]
    ::   /mention/(scot %p p.ref)/(scot %da q.ref)
    :: ==
  ++  bl-last
    ::  we'll perform the scry that we
    ::  previously had in the dude arm
    blab
    :: ?~  fug  blab
    :: =;  las=(pair @da writ:c)
    ::   blab(ref `id.q.las, con `content.q.las)
    :: %-  head  %~  tap  by
    :: .^    (map @da writ:c)
    ::     %gx
    ::   %+  welp  bla
    ::   /(scot %p p.u.fug)/[q.u.fug]/writs/newest/1/noun
    :: ==
  ++  bl-test
    ::  we'll test the last message in
    ::  the chat we're watching to see
    ::  if the message is a reminde-me
    blab
    :: ?~  con  blab
    :: ?.  ?=  $:  %story
    ::             [[%cite %chan [%chat @ @] *] *]
    ::             [@ [%break ~] *]
    ::         ==
    ::     u.con
    ::   blab
    :: =;  par
    ::   ?~  wat=(rush (head q.p.u.con) par)  blab
    ::   blab(wen ``@da`(add now.bol +>.u.wat))
    :: ;~((glue ace) (jest '!remind-me') ;~(pfix sig crub:so))
  --
::  +arvo: handles on-arvo
++  arvo
  |=  [pol=(pole knot) sig=sign-arvo]
  ^+  dat
  ?+    pol  ~|(bad-arvo-wire/pol !!)
      [%eyre %connect ~]
    ?>  ?=([%eyre %bound *] sig)
    %.  dat
    ?:  accepted.sig  same
    (slog leaf/"%prod can't bind eyre - [%kick ~] to retry" ~)
  ::
      [%when now=@ %flag host=@ chat=@ %mention who=@ wen=@ ~]
    ?>  ?=([%behn %wake *] sig)
    =+  now=`@da`(slav %da now.pol)
    =+  host=`@p`(slav %p host.pol)
    =+  chat=`@tas`(slav %tas chat.pol)
    =+  who=`@p`(slav %p who.pol)
    =+  wen=`@da`(slav %da wen.pol)
    =/  cit=cite:c
      :+  %chan  [%chat [host chat]]
      /msg/[who.pol]/(scot %ud wen)
    =/  act=action:c
      :^  [host chat]  now.bol  %writs
      :-  [our.bol now.bol]
      :^  %add  ~  our.bol
      :-  now.bol
      [%story [cite+cit]~ ~['reminder!' break+~]]       :: XX: ship+our.bol wen?
    =.  about
      (~(del ju about) [host chat] [now [who wen]])
    =?    about
        ?=(~ (~(got by about) [host chat]))
      (~(del by about) [host chat])
    %-  emit
    [%pass /reminder %agent cha %poke chat-action+!>(act)]
  ==
--