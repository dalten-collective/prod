::
::
  :: [ %story
  ::     p
  ::   [   p
  ::     ~[
  ::       [ %cite
  ::           cite
  ::         [%chan nest=[p=%chat q=[p=~zod q=%test-chat]] wer=/msg/~zod/170.141.184.505.918.841.323.162.627.759.026.009.538]
  ::       ]
  ::     ]
  ::     q=~['!remind-me eod ' [%break ~]]
  ::   ]
/-  *prod, c=chat
/+   r=rudder, dbug, default-agent, verb
/~  pages  (page:r tack poke)  /app/prod
|%
::
+$  versioned-state  $%(state-0)
::
+$  state-0
  $:  %0
      about=tack
  ==
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
    ?.  ?=(%handle-http-request mar)  `this
    =^  cards  state
      abet:(poke:eng mar vaz)
    [cards this]
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
    =^  cards  state
      abet:(arvo:eng wir sig)
    [cards this]
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
    cha  [our.bol %chat]
++  emit  |=(=card dat(dek [card dek]))
++  emil  |=(lac=(list card) dat(dek (welp lac dek)))
++  abet
  ^-  (quip card _state)
  [(flop dek) state]
::
++  eyre
  %-  emit
  =-  [%pass /eyre/connect %arvo %e -]
  [%connect [[~ [%apps %prod ~]] dap.bol]]
::
++  hear
  %-  emit
  [%pass /chats %agent [our.bol %chat] %watch /briefs]
::
++  init
  ^+  dat
  eyre:hear
::
++  peer
  |=  =path
  ?:  ?=([%http-response *] pat)  dat
  ~|(bad-peer-path/path !!)
::
++  load
  |=  vaz=vase
  ^+  dat
  ?>  ?=([%0 *] q.vaz)
  =.  state  !<(state-0 vaz)
  eyre
::
++  skip
  |=  act=^poke
  |^  ^-  $@(@t [brief:r (list card) tack])
    ?-    -.act
      %kick  ['rebooting' dek:eyre:hear ~]
      %drop  ['removing' (drop +.act)]
    ==
  ++  drop
    |=  (pair flag @da)
    ^-  (quip card tack)
    ?~  minds=(~(get by about) p)  [~ about]
    ?~  ment=(~(get by u.minds) q)  [~ about]
    =;  wir=path
      :_  (~(del ju about) p [q u.ment])
      [%pass wir %arvo %b [%rest q]]~
    ;:  weld  
      /when/(scot %da q)
      /flag/(scot %p p.p)/[q.p]
      /mention/(scot %p -.u.ment)/(scot %da +.u.ment)
    ==
  --
::
++  poke
  |=  [mar=mark vaz=vase]
  ?+    mar  ~|(bad-mark-poke/mar !!)
      %handle-http-request
    =;  out=(quip card tack)
      (emil(about +.out) (flop -.out))
    %.  [bol !<(order:r vaz) about]
    %:  (steer:r tack ^poke)
      pages
    ::
      |=  =trail:r
      ^-  (unit place:r)
      ?~  site=(decap:r /apps/prod site.trail)  ~
      ?+  u.site  ~
        ~       `[%page [& %prod]]
        [%$ ~]  `[%away /apps/prod]
      ==
    ::
      |=  =order:r
      ^-  [[(unit reply:r) (list card)] tack]
      =;  msg=@t  [[`code+[404 msg] ~] about]
      (rap 3 ~['%prod page ' url.request.order ' not found.'])
    ::
      skip
    ==
  ==
::
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
::
++  dude
  |=  [pol=(pole knot) sig=sign:agent:gall]
  ^+  dat
  ?+    pol  ~|(bad-dude-wire/pol !!)
      [%reminder ~]
    ?.  ?=(%poke-ack -.sig)  dat
    %.  dat
    ?~(p.sig same (slog leaf/"%prod had trouble" u.p.sig))
  ::
      [%chats ~]
    ?+  -.sig  dat
      %kick  hear
    ::
        %fact
      ?:  ?=(%chat-leave p.cage.sig)
        =+  flag=!<(flag q.cage.sig)
        dat(about (~(del by about) flag))
      ?.  ?=(%chat-brief-update p.cage.sig)  dat
      =/  upd  !<([whom:c brief:briefs:c] q.cage.sig)
      =^  cards  state
        bl-abet:bl-behn:bl-test:bl-last:(bl-prep:bl upd)
      (emil cards)
    ::
        %watch-ack
      %.  dat
      ?~  p.sig  same
      %-  slog                                          :: XX: and clean about?
      :_  u.p.sig
      leaf/"%prod can't see chat - [%kick ~] to retry"
    ==
  ==
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
  ++  bl-abet
    [(flop caz) state]
  ++  bl-prep
    |=  [w=whom:c *]
    ?.(?=(%flag -.w) blab blab(fug `p.w))
  ++  bl-behn
    ?~  wen  blab
    =+  flg=(need fug)
    =+  ref=(need ref)
    =;  wir=path
      %.  [%pass wir %arvo %b [%wait u.wen]]
      bl-emit(about (~(put ju about) flg [u.wen ref]))
    ;:  weld  
      /when/(scot %da u.wen)
      /flag/(scot %p p.flg)/[q.flg]
      /mention/(scot %p p.ref)/(scot %da q.ref)
    ==
  ++  bl-last
    ?~  fug  blab
    =;  las=(pair @da writ:c)
      blab(ref `id.q.las, con `content.q.las)
    %-  head  %~  tap  by
    .^    (map @da writ:c)
        %gx
      %+  welp  bla
      /(scot %p p.u.fug)/[q.u.fug]/writs/newest/1/noun
    ==
  ++  bl-test
    ?~  con  blab
    ?.  ?=  $:  %story
                [[%cite %chan [%chat @ @] *] *]
                [@ [%break ~] *]
            ==
        u.con
      blab
    =;  par
      ?~  wat=(rush (head q.p.u.con) par)  blab
      blab(wen ``@da`(add now.bol +>.u.wat))
    ;~((glue ace) (jest '!remind-me') ;~(pfix sig crub:so))
  --
--