      real function limit(p1,p2,x)
c
c ... limiter functionn
c
c     p1  :  lower bound
c     p2  :  upper bound
c     x   :  input
c
      if (x.le.p1) then
         limit = p1
      else
         if (x.lt.p2) then
            limit = x
         else
            limit = p2
         end if
      end if
      return
      end
      real function insw(expr0,expr1,expr2)
c
c...  input switch routine switches output between 2 inputs,
c     expr1 and expr2 based on the input expr0 less than
c      vs greater than or equal to zero
c
      if (expr0.lt.0) then
         insw = expr1
      else
         insw = expr2
      end if
      return
      end
      subroutine eqofmot
c
c     equations of motion.
c
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common / xintgl/  zz0021  ,zz0024
     1,zz0027  ,zz0030  ,zz0033  ,zz0036  ,pii     ,delhd   ,thendd
     1,thmldd  ,thmrdd  ,zbnadd  ,zz0147  ,xbnadd  ,zz0148  ,zbmldd
     1,zz0149  ,xbmldd  ,zz0150  ,zbmrdd  ,zz0151  ,xbmrdd  ,zz0152
     1,qddt(2,20)
     1,deldd    ,zz0173   ,vipsd
     1,zz0174  ,thedd   ,zz0175  ,phidd   ,zz0176  ,zesfr1  ,zesfr9
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c     debug save
      save i,kmttab,knttab
c
c
c ... wheel rotation equations.
c ... nose wheel.
c
      thendd = (fnslip*fnt*rn)/int

c
c     left main wheel.

      thmldd = (fmslpl*fmtl*rml-brtorl)/imt
c

c
      if (iasym.eq.1) then
c ...    right main wheel.
         thmrdd = (fmslpr*fmtr*rmr-brtorr)/imt

      end if
c
c     unsprung mass equations.
c     nose gear.
c
      zbnadd = (fnt*costhe+fdnt*sinthe-fstal*cosgam+fstnl*singam)/mn
     $          -g*costhe

      xbnadd = (fdnt*costhe-fnt*sinthe-fstnl*cosgam-fstal*singam)/mn
     $          +g*sinthe

c     left main gear.
      zbmldd = (-fmgl+fdmtl*sinthe+fmtl*costhe)/mm-g*costhe

      xbmldd = (-fnmgl+fdmtl*costhe-fmtl*sinthe)/mm+g*sinthe

      if (iasym.eq.1) then
c        right main gear.
         zbmrdd = (-fmgr+fdmtr*sinthe+fmtr*costhe)/mm-g*costhe
         xbmrdd = (-fnmgr+fdmtr*costhe-fmtr*sinthe)/mm+g*sinthe
      end if
c
      if (irigid.eq.1) then
         do 253 i=1,ntm
 253     qdd(i)=0.
         if (time.ne.0.) go to 254
      else
c        modal equations.
         do 205 i=1,nsm
         j = 5*(i-1)
         qdd(i) = (dlf(1)*ce(j+1)+dlf(2)*ce(j+2)+(dlf(3)+dlf(5))*ce(j+3)
     $           +(dlf(4)+dlf(6))*ce(j+4)+dlf(7)*ce(j+5))/(two*gm(i))
     $           -gomeg(i)*qdot(i) -omsq(i)*q(i)
 205     continue
         if (iasym.eq.1) then
            do 206 i=1,nam
               k=nsm+i
               j=5*(k-1)
               qdd(k) = ((dlf(3)-dlf(5))*ce(j+3))/(two*gm(k))-gomeg(k)
     $                *qdot(k)-omsq(k)*q(k)
 206        continue
         end if
      end if
c
      do 350 i=1,ntm
         qddt(1,i) = qdd(i)
 350  continue
c
c     rigid vertical translation.
c
      thrust=(18800.+7.0532*vfps)*th
      if(kvtaxi .eq. 1)thrust=weight*rolfri+drag
 254  deldd = ((fmgl+fmgr+fngv)*costhe-(fnmgl+fnmgr+fngl-thrust)
     $ *sinthe+lift)/mass-g
c
c     rigid fore and afr translation.
      vipsd = -((fnmgl+fnmgr+fngl)*costhe+(fmgl+fmgr+fngv)*sinthe+drag
     $         -thrust*costhe)/mass
      dawd = thed+(deld*vipsd/vips**2-deldd/vips)/(one+(deld/vips)**2)
      aerom = -lift*dar+qs*cbar*(cm+cmh*delh+.5*cbar*cmq*dawd/vips)
c     rigid pitch rotation.
      thedd = (fngv*dn-(fmgr+fmgl)*dm-fngl*lns-fnmgl*lmsl-fnmgr*lmsr
     $  +thrust*dth+aerom)/ithe
c
      if (iasym.eq.1) then
c        rigid roll rotation.
         phidd = (fmgl-fmgr)*arml/iphi
      else
         phidd=0.
         phid=0.
         phi=0.
      end if
c
c endd of equations of motion.
c
c
      zz0147  =zbnad
      zz0148  =xbnad
      zz0149  =zbmald
      zz0150  =xbmald
      zz0151  =zbmard
      zz0152  =xbmard
c
      do 400 i=1,20
         qddt(2,i) = qdott(1,i)
 400  continue
c
      zz0173  =deld
      zz0174  =vips
      zz0175  =thed
      zz0176  =phid
c
      return
      end
      subroutine cset(i,value)
c
c   place value into appropriate common block associated with c array
c
      common /   timer/  c1(6)
      common /  integl/  c2(73)
      common /  xintgl/  c3(73)
      common / y0intgl/  c4(73)
      common / inputs0/  c5(72)
      common             c6(653)
c
c
c
check for error condition - branch if found or find appropriate location
c
c
      if ((i.le.0) .OR. (i.gt.944))  go to 100
      if (i.le.6) then
c
c           put in timer block
c
         c1(i) = value
         return
      else if (i.le.77) then
c

c           put in integl block
c
         c2(i-6) = value
         return
      else if (i.le.148) then
c
c           put in xintgl block
c
         c3(i-77) = value
         return
      else if (i.le.219) then
c
c           put in y0intgl block
c
         c4(i-148) = value
         return
      else if (i.le.291) then
c
c           put in inputs block
c
         c5(i-219) = value
         return
      else if (i.le.944) then
c
c           put in blank common
c
         c6(i-291) = value
         return
      end if
c
c    print error message and return
c
 100  call page(4)
      write(6,1000) i
 1000 format(1h0,i8,30hVARIABLE OUT OF RANGE IN CSET///)
      return
      end
      subroutine rang1(fname,qlow,qhigh)
c
      character *7 fname
      character *7  symb
      common / symbl/ symb(856)
c
      common / range/  irange(100)
c
      common / minmax/ rmin(100), rmax(100), trmin(100), trmax(100)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c     debug save
      save i,j,y
c
c
      do 800 i = 1,nosymb
         if (fname.eq.symb(i)) go to 900
 800  continue
 900  do 1000 j=1,krange
         if(i-irange(j))1000,1010,1000
1000  continue
1010  qlow=rmin(j)
      qhigh=rmax(j)
      if(qhigh.ne.qlow) return
      if (qlow.eq.0) then
         qlow=-1.
         qhigh=1.
         return
      else
         y=qlow
         i=0
1050     if(abs(y).ge.10.) then
            y=y/10.
            i=i+1
            go to 1050
         else
1070        if(abs(y).lt.1.) then
               y=y*10.
               i=i-1
               go to 1070
            end if
         end if
         qlow=qlow-10.**i
         qhigh=qhigh+10.**i
         return
      end if
      return
      end
      subroutine update
c
c asymmetric flexible taxi or landing dynamic response analysis
c procedure with longitudinal control system representation.  rigid
c vertical, longitudinal, pitch and roll degrees of freedom and up to
c 20 symmetric and/or antisymmetric normal mode degrees of freedom.
c * * * * * ewh, 11-23-81 * * * * * * correct thru 12-10-81
c
c
      call profile
      call gearfr
      call aerocs
      call eqofmot
      call respons
c
      return
      end
      subroutine initsys
c
c
      common /smtail/ismtail,smcltab(30),smcmtab(30),nsmcltb
     *,nsmcmtb,smclh,smcmh
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / y0intgl/  al0     ,zz0022
     1,thedb0  ,an0     ,resp0   ,zz0039  ,piic    ,delh0   ,thend0
     1,themd0  ,zz0177  ,zbnad0  ,zbna0   ,xbnad0  ,xbna0   ,zbmad0
     1,zbma0   ,xbmad0  ,xbma0   ,zz0178  ,zz0179  ,zz0180  ,zz0181
     1,qd0(2,20)        ,deld0   ,del0    ,vips0
     1,x0      ,thed0   ,the0    ,zz0144  ,zz0146  ,zesfr10 ,zesfr90
c
      character *7  symb
      common / symbl/ symb(856)
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c     debug save
      save i
c
c    system segment of model
c
      kpoint = 963
      nointg = 73
      nosymb = 530
      zz0019  =1.1
      if(ismtail .eq. 1)zz0019  = .1
      zz0022  =0.
      zz0025  =0.1
      zz0028  =0.02
      zz0031  =.05
      zz0034  =.25
      zz0035  =.0833
      zz0039  =0.
c
      do  100 i =1,20
         qd0(1,i) = 0.
         qd0(2,i) = 0.
 100  continue
c
c
      read (4,1000)  (symb(i), i=1,550)
 1000 format (10A7)
c
c
      return
      end
      subroutine initial
c
c begin  program initialization.
c   initial segment of model
      common /smtail/ismtail,smcltab(30),smcmtab(30),nsmcltb
     *,nsmcmtb,smclh,smcmh
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common /cleara/ fspta,wlpta,dipta
c
      common /speedbr/   cdsbrnd ,cdsbr0
c
      common / y0intgl/  al0     ,zz0022
     1,thedb0  ,an0     ,resp0   ,zz0039  ,piic    ,delh0   ,thend0
     1,themd0  ,zz0177  ,zbnad0  ,zbna0   ,xbnad0  ,xbna0   ,zbmad0
     1,zbma0   ,xbmad0  ,xbma0   ,zz0178  ,zz0179  ,zz0180  ,zz0181
     1,qd0(2,20)        ,deld0   ,del0    ,vips0
     1,x0      ,thed0   ,the0    ,zz0144  ,zz0146  ,zesfr10 ,zesfr90
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      common  / functs/  bumtal(30) ,srampl(30)  ,fritab(30)
     1,bumtab(30) ,sramp(30) ,cltab(30) ,cmtab(30) ,cdtab(30)
     1,fantab(30)  ,knttab(30)  ,fcntab(30)  ,slptab(30)
     1,kmttab(30)  ,fcmtab(30)  ,famtab(30)  ,znttab(30)
     1,zmttab(30)  ,santab(30)  ,samtab(30)  ,esfvpc(16)
     1,vpcesf(16), nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
c
      common / flags/ iflag(50)
c
      equivalence  (line, iflag(43))
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c     debug save
      save cdtag0,i,list0
c
c     initialize bump routine
c
      call elevat
      call smtlin(cltab,cmtab,ncltab,ncmtab,clh,cmh)
c
c compute geometric constants common to all configurations.
      dntm=fsmg0-fsng
      h0=rm0-rn0-wlmg+wlng
      stv=wlst-wlng
      stl=fsst-fsng
      dbv=wldb-dbwl
      dbl=fsdb-dbfs
      ndn=fsdb-fsst
      xds=fsst-dbfs
      zds=wlst-dbwl
      lsl=sqrt((dbfs-fsng-dax)**2+(dbwl-wlng)**2)
      bet0=atan(dbl/dbv)
      lsu=sqrt(xds**2+zds**2)
      gam0=atan(xds/zds)
      cosgam=cos(gam0)
      singam=sin(gam0)
      cosbet=cos(bet0)
      sinbet=sin(bet0)
      lsubmg=sin(bet0-gam0)*lsu
      cosbmg=cos(bet0-gam0)
c compute modal coefficients for specific configuration.
      ntm=nsm+nam
      do 10 i=1,ntm
      omsq(i)=omeg(i)**2
10    gomeg(i)=gdamp*omeg(i)
c compute constants for specific configuration.
      waplg=weight-g*(two*mm+mn)
      dn0=fscg-fsng
      dm0=fsmg0-fscg
      lnsu=wlcg-wlng
      lmsu=wlcg-wlmg
      dth=wlcg-wlth
      dar=fs35c-fscg
c test for landing simulation, if yes, go to landing initialization.
      if(landg.gt.0)go to 11
c begin taxi initialization.
      al0=0.
      dn=dn0
      lns=lnsu
      lms=lmsu
      fdnt=0.
      fdmt=0.
      the0=0.
      cos0=1.
      sin0=0.
      omcos0=0.
      vtd=vtaxi*1.69
      vips0=vtd*foot
      vfps=vtd
      deld0=0.
      qs0=dens*s*(vtd**2)/two
c
c iterative loop to equilibrate airplane in taxi condition.
      do 105 i=1,15
c lift and pitching moment coefficients.
      cl0 = funct(cltab,ncltab,'CLTAB ',al0)
      cm0 = funct(cmtab,ncmtab,'CMTAB ',al0)
c initial horizontal tail deflection.
      delh0=delth0
c airplane lift and pitching moment.
      lift0=qs0*(cl0+clh*delh0)
      momen0=-list0*dar+qs0*cbar*(cm0+cmh*delh0)-fdnt*lns-two*fdmt*lms
      thrust=(18800.+7.0532*vfps)*th
      cdtag0=funct(cdtab,ncdtab,'CDTAB ',cl0)
      cdtag0=cdtag0+cdsbrnd
      drag=qs0*cdtag0
      if(kvtaxi .eq. 1)thrust=weight*rolfri+drag
      fnt=((waplg-lift0)*(dm0-lms*sin0)-dth*thrust-momen0)/(dm0+dn)+g*mn
c calculate tire and gear forces.
      fdnt=rolfri*fnt
      fngp=fnt-g*mn
      fngv=fngp*cos0+fdnt*sin0
      fmt=(waplg-lift0-fngp)/two+g*mm
      fdmt=(rolfri+brfri)*fmt
      fmg=(fmt-g*mm)*cos0+fdmt*sin0
c nose and main tire deflections.
      znt = funct(znttab,nznttab,'ZNTTAB',fnt)
c calculate tire and gear forces.
      zmt = funct(zmttab,nzmttab,'ZMTTAB',fmt)
c additional strut forces.
      fstal=fngp*cos(gam0+the0)+fdnt*sin(gam0+the0)
      fstnl=-fngp*sin(gam0+the0)+fdnt*cos(gam0+the0)
      fnmg=-(fmt-g*mm)*sin0+fdmt*cos0
c nose and main gear strokes.
      nstrk = funct(santab,nsantab,'SANTAB',fstal)
      mstrk = funct(samtab,nsamtab,'SAMTAB',fmg)
c strut normal deflections.
      snn=fstnl/kd
      dxbma=fnmg/knmg
c nose gear support structure compliance.
      zstf=.000013*fngv
      zdbf=.0000046*fngv
c strut defledted geometry calculations.
      snv=nstrk*cosgam-snn*singam+zstf
      lns=lnsu-snv
      snl=nstrk*singam+snn*cosgam+(zdbf-zstf)*lns/ndn
      dn=dn0-snl
      lms=lmsu-mstrk
c calculation of airplane pitch attitude.
      deeone=lns-lms
      lata=dn+dm0
      th1=asin(deeone/lata)
      hdel=rn0+znt-rm0-zmt
      th2=asin(hdel/lata)
      the0=th1+th2
      cos0=cos(the0)
      sin0=sin(the0)
      omcos0=one-cos0
c angle of attack for aerodynamic force calculation.
      al0=the0*degpr
c write statements to check on iteration convergence.  (omit from
c final program.)
c
      line = line + 5
c      write(6,90)nstrk,snn,dxbma
c      write(6,91)snv,snl,hdel
c      write(6,92)the0,lms,mstrk
c      write(6,93)fnt,fmt,fdmt
c
      if (line.ge.55)  call page(1)
c
105   continue
c end of iterative loop.
90    format(" NSTRK= ",1p,e14.5," SNN= ",e14.5," DXBMA= ",e14.5)
91    format(" SNV=   ",1p,e14.5," SNL= ",e14.5," HDEL=  ",e14.5)
92    format(" THE0=  ",1p,e14.5," LMS= ",e14.5," MSTRK= ",e14.5)
 93   format(" FNT=   ",1p,e14.5," FMT= ",e14.5," FDMT=  ",e14.5/)
c
c airplane center of gravity deflection.
      del0=-lmsu*omcos0-mstrk*cos0+dxbma*sin0+dm0*sin0+zmt
c nose tire initial rolling radius and velocity.
      rn=rn0+znt
      thend0=vips0/rn
c main tire initial rolling radius and velocity.
      rm=rm0+zmt
      themd0=vips0*(1.-brfri/11.)/rm
c set initial input for alpha to flight control system equal to zero.
      al0=0.
c endd of initialization unique to taxi analysis.
      go to 104
11    continue
c begin initialization unique to landing analysis.
c set initial input to flight control system for alpha.
      al0=alpha
c calculate angle at which nose tire would touch ground with main
c tire tangent to ground and struts fully extended.
      nthe=atan((lnsu-lmsu)/(dn0+dm0))+asin((rn0-rm0)/sqrt((dn0+dm0)**2+
     $(lnsu-lmsu)**2))
c lift and moment coefficients.
      cl0 = funct(cltab,ncltab,'CLTAB ',al0)
      cm0 = funct(cmtab,ncmtab,'CMTAB ',al0)
c calculate initial horizontal tail angle for pitch equilibrium.
      delh0=(cbar*cm0-cl0*dar)/(-cmh*cbar+clh*dar)
c calculate dynamic pressure times reference area required for
c equilibrium.
      qs0=weight/(cl0+clh*delh0)
c calculate airplane landing speed.
      vtd=sqrt((two*qs0)/(dens*s))
      vips0=vtd*foot
c airplane pitch attitude.
      pitch=alpha-degpr*(vsink/vtd)
      the0=pitch/degpr
      cos0=cos(the0)
      sin0=sin(the0)
      omcos0=one-cos0
c initial vertical velocity.
      deld0=-vsink*foot
c calculate strut deflections for airplane off ground condition.
      mstrk=(-fam0-g*mm*cos0)/ksem
      lms=lmsu-mstrk
      dxbma=g*mm*sin0/knmg
      nstrk=-(fan0+g*mn*cos(gam0+the0))/ka
      snn=g*mn*sin(gam0+the0)/kd
      lns=lnsu-nstrk*cosgam+snn*singam
      fngv=-mn*g*cos0
c nose gear support structure compliance.
      zstf=.000013*fngv
      zdbf=.0000046*fngv
      snv=nstrk*cosgam-snn*singam+zstf
      snl=nstrk*singam+snn*cosgam+(zdbf-zstf)*lns/ndn
      dn=dn0-snl
c test pitch attitude to determine which gear(s) contact ground.
c
      if (the0.lt.nthe) then
c nose tire is tangent to ground.
      del0=-lnsu*omcos0-snv*cos0-dn*sin0+h0
      else
c main tires (or main and nose tires) are tangent to ground.
12    del0=-lmsu*omcos0-mstrk*cos0+(dxbma+dm0)*sin0
      end if
c
c calculate nose and main strut forces.
13    fstal=-mn*g*cos(gam0+the0)
      fstnl=mn*g*sin(gam0+the0)
      fmg=-mm*g*cos0
      fnmg=mm*g*sin0
c remaining initialization common to taxi and landing analysis.
c mass of airplane less unsprung portions of gear.
104   mass=weight/g-two*mm-mn
c calculate additional gear force components.
      axtdb=lsl-nstrk+dax*singam
      fdba=(fstnl*(axtdb+lsu)-fstal*dax*cosgam)/lsubmg
      fstau=fstal-fdba*cosbmg
      fstnu=(fstnl*axtdb-fstal*dax*cosgam)/lsu
      fstv0=fstau*cosgam+fstnu*singam
      fstl0=fstau*singam-fstnu*cosgam
      fdbv0=fdba*cosbet
      fdbl0=fdba*sinbet
      fnmg0=fnmg
      fmdba=fnmg*lms/mdbarm
      fmdbv0=-mdbasn*fmdba
      fmsv0=fmg-fmdbv0
c gear unsprung mass initial velocities.
      zbnad0=deld0*cos0
      zbmad0=zbnad0
      xbnad0=-deld0*sin0
      xbmad0=xbnad0
c gear unsprung mass initial displacements.
52    zbna0=(lnsu+del0)*cos0+dn0*sin0-lns+zstf
      zbma0=(lmsu+del0)*cos0-dm0*sin0-lms
      xbna0=snl-(del0+lnsu)*sin0-dn0*omcos0
      xbma0=dxbma-(del0+lmsu)*sin0+omcos0*dm0
c initialization of flight control system components.
      mgsw=2.-mstrk
      thedb0=insw(mgsw,0.,6.)
      resp0=.322*(al0-thedb0)
      gcom=3.*resp0
c      write(6,*)' RESP0 GCOM AL0 THEDB0',resp0,gcom,al0,thedb0
      if(landg .ne. 0)call contrli (mgsw)
c      write(6,*)' RESP0 GCOM AL0 THEDB0',resp0,gcom,al0,thedb0
      piic=.2*(delh0-.42*al0)
      if(ismtail .eq. 1)piic=.2*(delh0-.5*al0)
      x0=xf0*foot
      dipta=wlpta-wlmg+rm0
c
c endd of initialization section.
c
      zz0177  =themd0
      zz0178  =zbmad0
      zz0179  =zbma0
      zz0180  =xbmad0
      zz0181  =xbma0
c
      return
      end
      subroutine gearfr
c
c
c calculation of tire forces.
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      common  / functs/  bumtal(30) ,srampl(30)  ,fritab(30)
     1,bumtab(30) ,sramp(30) ,cltab(30) ,cmtab(30) ,cdtab(30)
     1,fantab(30)  ,knttab(30)  ,fcntab(30)  ,slptab(30)
     1,kmttab(30)  ,fcmtab(30)  ,famtab(30)  ,znttab(30)
     1,zmttab(30)  ,santab(30)  ,samtab(30)  ,esfvpc(16)
     1,vpcesf(16), nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c     debug save
      save i,rafror,xmard,zmtld,zmtld1,zmtrd,zmtrd1
c
c nose wheel unsprung mass deflections in ground axis.
421   costhe=cos(the)
      sinthe=sin(the)
      omcos=one-costhe
      zna=-xbna*sinthe+zbna*costhe
      znad=-xbnad*sinthe+zbnad*costhe
      height=zna+h0-bumpn
c tire compression and compression rate.
      znt=fcnsw(height,height,0.,0.)
      zntd1=znad-rafrin*vips
      zntd=fcnsw(height,zntd1,0.,0.)
c tire rolling radius.
      rn=rn0+znt
      fnt = funct(knttab,nknttab,'KNTTAB',znt)-ctd*zntd
      if(fnt.lt.0.)fnt=0.
      xnad=xbnad*costhe+zbnad*sinthe
c tire slip ratio.
      ntslip=(vips-xnad-rn*thend)/vref
      fnslip = funct(slptab,nslptab,'SLPTAB',ntslip)
c tire drag force.
      fdnt=(fnslip+rolfri+rafrin)*fnt
c left main wheel - unsprung mass deflections in ground axis.
      zmal=-xbmal*sinthe+zbmal*costhe-bumpml
c tire compression.
      zmtl=fcnsw(zmal,zmal,0.,0.)
      zmtld1=-xbmald*sinthe+zbmald*costhe-rafril*vips
      zmtld=fcnsw(zmal,zmtld1,0.,0.)
      rml=rm0+zmtl
      fmtl = funct(kmttab,nkmttab,'KMTTAB',zmtl)-ctd*zmtld
      if(fmtl .lt. 0.)fmtl=0.
      xmald=xbmald*costhe+zbmald*sinthe
c tire slip ratio.
      mtslpl=(vips-xmald-rml*themld)/vref
      fmslpl = funct(slptab,nslptab,'SLPTAB',mtslpl)
c tire drag force.
      fdmtl=(fmslpl+rolfri+rafril)*fmtl
c brake torque.
      brtorl=brfri*rml*fmtl
      if(themld.lt.0.)brtorl=0.
c
      if (iasym.eq.1) then
c right main wheel - unsprung mass deflections in ground axis.
60    zmar=-xbmar*sinthe+zbmar*costhe-bumpmr
c tire compression.
      zmtr=fcnsw(zmar,zmar,0.,0.)
      rmr=rm0+zmtr
      zmtrd1=-xbmard*sinthe+zbmard*costhe-rafror*vips
      zmtrd=fcnsw(zmar,zmtrd1,0.,0.)
      fmtr = funct(kmttab,nkmttab,'KMTTAB',zmtr)-ctd*zmtrd
      xmard=xbmard*costhe+zbmard*sinthe
c tire slip ratio.
      mtslpr=(vips-xmard-rmr*themrd)/vref
      fmslpr = funct(slptab,nslptab,'SLPTAB',mtslpr)
c tire drag force.
      fdmtr=(fmslpr+rolfri+rafrir)*fmtr
c brake torque.
      brtorr=brfri*rmr*fmtr
      if(themrd.lt.0.)brtorr=0.
      else
      fmtr = fmtl
      fdmtr=fdmtl
      end if
c
c  calculate landing gear strut forces.
c calculate nose gear fuselage local deflections.
62    zstf=.000013*fngv
      zdbf=.0000046*fngv
      if(irigid.eq.1)go to 300
c rename unsubscripted modal velocities and displacements.
c
      do 50 i=1,ntm
         qdot(i) = qdott(1,i)
         q(i) = qdott(2,i)
 50   continue
c
c calculate gear attachment point deflections and velocities due
c to modal coordinate response.
      do 20 i=1,5
      dld(i)=0.
      dldd(i)=0.
      adld(i)=0.
20    adldd(i)=0.
c symmetric components.
      do 21 i=1,nsm
      j=5*(i-1)
      do 21 k=1,5
      dld(k)=dld(k)+q(i)*ce(j+k)
21    dldd(k)=dldd(k)+qdot(i)*ce(j+k)
c
      if (iasym.ne.1) go to 64
c antisymmetric components.
c
      do 22 i = 1,nam
      l=nsm+i
      j=5*(l-1)
      do 22 k=1,5
      adld(k)=adld(k)+q(l)*ce(j+k)
22    adldd(k)=adldd(k)+qdot(l)*ce(j+k)
      go to 302
300   do 301 i=1,5
      dld(i)=0.
      dldd(i)=0.
      adld(i)=0.
301   adldd(i)=0.
c left and right gear components.
302   dld3=dld(3)+adld(3)
      dld5=dld(3)-adld(3)
      dld4=dld(4)+adld(4)
      dld6=dld(4)-adld(4)
      dldd3=dldd(3)+adldd(3)
      dldd5=dldd(3)-adldd(3)
      dldd4=dldd(4)+adldd(4)
      dldd6=dldd(4)-adldd(4)
      go to 30
64    dld3=dld(3)
      dld4=dld(4)
      dldd3=dldd(3)
      dldd4=dldd(4)
c calculate nose strut deflected geometry.
30    snv=zbna-del*costhe-dn*sinthe+lnsu*omcos-zstf-dld(1)
      lns=lnsu-snv
      snl=xbna+dn*omcos+(lnsu+del)*sinthe-(zdbf-zstf+dld(1)-dld(2))*lns/
     $ndn
      dn=dn0-snl
      sna=snl*singam+snv*cosgam
      nstrk=fcnsw(sna,0.,0.,sna)
      snvd=zbnad-deld*costhe+del*thed*sinthe-dn*thed*costhe+lnsu*thed*
     $    sinthe-dldd(1)
      snld=xbnad+(dn*thed+deld)*sinthe+(lnsu+del)*thed*costhe-(dldd(1)-
     $     dldd(2))*lns/ndn
      snad=snld*singam+snvd*cosgam
      snn=snl*cosgam-snv*singam
      snnd=snld*cosgam-snvd*singam
c calculate nose gear force components.
      fstnl=snn*kd+snnd*cd
      ff1=abs(mustr*fstnl*(2.*(29.17/(5.66+nstrk))-1.))
      ff2 = ff1* funct(fritab,nfritab,'FRITAB',snad)
      fan = funct(fantab,nfantab,'FANTAB',sna)
      fcn = funct(fcntab,nfcntab,'FCNTAB',sna)
      snada=abs(snad)
      fst1=fan+fcn*snad*snada+ff2
      fst2=fan0+sna*ka+snad*ca
      fstal=fcnsw(sna,fst2,fst1,fst1)
      axtdb=lsl-nstrk+dax*singam
      fdba=(fstnl*(axtdb+lsu)-fstal*dax*cosgam)/lsubmg
      fstau=fstal-fdba*cosbmg
      fstnu=(fstnl*axtdb-fstal*dax*cosgam)/lsu
      fstv=fstau*cosgam+fstnu*singam
      fstl=fstau*singam-fstnu*cosgam
      fdbv=fdba*cosbet
      fdbl=fdba*sinbet
      fngv=fstv+fdbv
      fngl=fstl+fdbl
c calculate left gear deflected geometry.
      sml=zbmal-(del+lmsu)*costhe+lmsu+dm0*sinthe-arml*phi-dld3
      lmsl=lmsu-sml
      dm=dm0
      smdl=zbmald-deld*costhe+(del+lmsu)*thed*sinthe+dm0*thed*costhe-
     $   arml*phid-dldd3
      smdla=abs(smdl)
      mstrkl=fcnsw(sml,0.,0.,sml)
c calculate left gear force components.
      fcml = funct(fcmtab,nfcmtab,'FCMTAB',sml)
      faml = funct(famtab,nfamtab,'FAMTAB',sml)
      fm1l=faml+fcml*smdl*smdla
      fm2l=fam0+ksem*sml+csem*smdl
      fmgl=fcnsw(sml,fm2l,fm1l,fm1l)
      dxbmal=xbmal+(del+lmsu)*sinthe-dm*omcos-(dld3-dld4)*lmsl/mdn
      dxbmld=xbmald+(del+lmsu)*thed*costhe+(deld-dm*thed)*sinthe-(dldd3-
     $dldd4)*lmsl/mdn
      fnmgl=knmg*dxbmal+cnmg*dxbmld
c
      if (iasym.ne.1) then
      fmgr=fmgl
      fnmgr=fnmgl
      lmsr=lmsl
      mstrkr=mstrkl
      else
c calculate right gear deflected geometry.
65    smr=zbmar-(del+lmsu)*costhe+lmsu+dm0*sinthe+arml*phi-dld5
      lmsr=lmsu-smr
      smdr=zbmard-deld*costhe+(del+lmsu)*thed*sinthe*dm0*thed*costhe+
     $   arml*phid-dldd5
      smdra=abs(smdr)
      mstrkr=fcnsw(smr,0.,0.,smr)
c calculate right gear force components.
      fcmr = funct(fcmtab,nfcmtab,'FCMTAB',smr)
      famr = funct(famtab,nfamtab,'FAMTAB',smr)
      fm1l=famr+fcmr*smdr*smdra
      fm2l=fam0+ksem*smr+csem*smdr
      fmgr=fcnsw(smr,fm2r,fm1r,fm1r)
      dxbmar=xbmar+(del+lmsu)*sinthe-dm*omcos-(dld5-dld6)*lmsr/mdn
      dxbmrd=xbmard+(del+lmsu)*thed*costhe+(deld-dm*thed)*sinthe-(dldd5-
     $dldd6)*lmsr/mdn
      fnmgr=knmg*dxbmar+cnmg*dxbmrd
      fmdbvr=-mdbasn*fnmgr*lmsr/mdbarm
      end if
c
66    fmdbvl=-mdbasn*fnmgl*lmsl/mdbarm
      if(irigid.eq.1)go to 70
c calculate incremental forces for modal excitation.
      dlf(1)=fstv-fstv0
      dlf(2)=fdbv-fdbv0
      dlf(3)=fmgl-fmdbvl-fmsv0
      dlf(4)=fmdbvl-fmdbv0
      if(iasym.eq.1)go to 67
      go to 68
67    dlf(5)=fmgr-fmdbvr-fmsv0
      dlf(6)=fmdbvr-fmdbv0
      dlf(7)=fngl+fnmgl+fnmgr-fstl0-fdbl0-two*fnmg0
      go to 70
68    dlf(5)=dlf(3)
      dlf(6)=dlf(4)
      dlf(7)=fngl-fstl0-fdbl0+2.*(fnmgl-fnmg0)
c
 70   return
      end
      subroutine aerocs
c
c
c aerodynamics and control system equations.
c
      common /smtail/ismtail,smcltab(30),smcmtab(30),nsmcltb
     *,nsmcmtb,smclh,smcmh
c
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common /speedbr/   cdsbrnd ,cdsbr0
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common / xintgl/  zz0021  ,zz0024
     1,zz0027  ,zz0030  ,zz0033  ,zz0036  ,pii     ,delhd   ,thendd
     1,thmldd  ,thmrdd  ,zbnadd  ,zz0147  ,xbnadd  ,zz0148  ,zbmldd
     1,zz0149  ,xbmldd  ,zz0150  ,zbmrdd  ,zz0151  ,xbmrdd  ,zz0152
     1,qddt(2,20)
     1,deldd    ,zz0173   ,vipsd
     1,zz0174  ,thedd   ,zz0175  ,phidd   ,zz0176  ,zesfr1  ,zesfr9
c
      common / y0intgl/  al0     ,zz0022
     1,thedb0  ,an0     ,resp0   ,zz0039  ,piic    ,delh0   ,thend0
     1,themd0  ,zz0177  ,zbnad0  ,zbna0   ,xbnad0  ,xbna0   ,zbmad0
     1,zbma0   ,xbmad0  ,xbma0   ,zz0178  ,zz0179  ,zz0180  ,zz0181
     1,qd0(2,20)        ,deld0   ,del0    ,vips0
     1,x0      ,thed0   ,the0    ,zz0144  ,zz0146  ,zesfr10 ,zesfr90
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      common  / functs/  bumtal(30) ,srampl(30)  ,fritab(30)
     1,bumtab(30) ,sramp(30) ,cltab(30) ,cmtab(30) ,cdtab(30)
     1,fantab(30)  ,knttab(30)  ,fcntab(30)  ,slptab(30)
     1,kmttab(30)  ,fcmtab(30)  ,famtab(30)  ,znttab(30)
     1,zmttab(30)  ,santab(30)  ,samtab(30)  ,esfvpc(16)
     1,vpcesf(16), nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c     debug save
      save clt,i
c
c
c airplane velocity.
70    vfps=vips/foot
      vknots=vfps/1.69
c angle of attack.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
c lift and moment coefficients.
      cl = funct(cltab,ncltab,'CLTAB ',alp)
      cm = funct(cmtab,ncmtab,'CMTAB ',alp)
c dynamic pressure times reference area.
      qs=dens*s*vfps**2/two
c test to see if control system is to be represented.
      if(sas)200,201,200
c constant horizontal tail angle.
200   delh=delh0
      go to 202
c weight on wheels switch.
201   mgsw=2.-mstrkl
c angle of attack input and feedback.
      alp2=11.*(alp-.909*alpl)
c angle of attack switch.
      if(ismtail .eq. 1)alp2=alp
      alpf=insw(mgsw,0.,alp2)
c angle of attack lag functionn.
c     alpl       =intgrl     (al0        ,zz0021     )
c pitch acceleration in degrees per second squared.
      zz0021     =(alpf-alpl)/zz0019
      theddd=thedd*degpr
c rigid normal acceleration.
      deldd1=deldd*costhe-vipsd*sinthe+155.*thedd
c
      if (irigid.ne.1) then
c add modal pitch and normal acceleration contributions.
      do 250 i=1,nsm
      theddd=theddd+csc(i)*qdd(i)
            deldd1 = deldd1 + csc(nsm+1) * qdd(i)
 250     continue
      end if
c
c convert normal acceleration to g's.
251   delddg=deldd1/g
c pitch acceleration lag functionn.
c     thdw0      =intgrl     (zz0022     ,zz0024     )
c pitch bias for weight on wheels.
      zz0024     =(theddd-thdw0)/one
      thedb=insw(mgsw,0.,6.)
c pitch bias lag functionn.
c     thedb1     =intgrl     (thedb0     ,zz0027     )
c pitch gain changer for weight on wheels.
      zz0027     =(thedb-thedb1)/zz0025
      prin=insw(mgsw,.461,.334)
c normal acceleration lag functionn.
c     glag       =intgrl     (an0        ,zz0030     )
      zz0030     =(delddg-glag)/zz0028
      resp=glag+prin*thdw0+.322*(alpl+.7*thdw0-thedb1)
c     resp1      =intgrl     (resp0      ,zz0033     )
      zz0033     =(resp-resp1)/zz0031
      resp2=insw(mgsw,resp1,resp)
      zz0036=(resp2-zz0037)/zz0035
c     zz0037     =intgrl     (zz0039     ,zz0036     )
      dhcom1=zz0034*zz0036+zz0037
      if(landg .ne. 0)call control (mgsw)
      dhcom2=1.26*(dhcom1-gcom)
      if(ismtail .eq. 1)dhcom2=1.50*(dhcom1-gcom)
      alpk=.42*alpl
      if(ismtail .eq. 1)alpk=.50*alpl
      pi5=5.*pi
      pil=deadsp(-21.,21.,pi5)
      if(ismtail .eq. 1)pil=deadsp(-25.,25.,pi5)
c horizontal tail command.
      pk=pi5+dhcom2+alpk
      pl=deadsp(-21.,21.,pk)
      if(ismtail .eq. 1)pl=deadsp(-25.,25.,pk)
      pia=-5.*pl+dhcom2
      pig=-.5*pi5
      piii=insw(mgsw,pig,pia)
      pii=piii-50.*pil
c     pi         =intgrl     (piic       ,pii        )
c     dela       =intgrl     (delh0      ,delhd      )
      dlhd=(pk-delh)/.05
c rate limiter.
      delhd=limit(-50.,50.,dlhd)
      if(ismtail .eq. 1)delhd=limit(-60.,60.,dlhd)
c horizontal tail deflection.
      delh=limit(-21.,21.,dela)
      if(ismtail .eq. 1)delh=limit(-25.,25.,dela)
c endd of control system.
c
202   continue
c lift and drag.
      lift=qs*(cl+clh*delh)
      clt=cl+clh*delh
      cd1 = funct(cdtab,ncdtab,'CDTAB ',clt)
      if(nstrk .gt. 2.)then
      cd1=cd1+cdsbrnd
      else
      cd1=cd1+cdsbr0
      end if
      cdrag=cd1-.0055*delh*clt
      drag=qs*cdrag
c endd of aerodynamic calculations.
c
c
      return
      end subroutine aerocs
      function fcnsw(expr0,expr1,expr2,expr3)
c...  functionn switch routine switches output between 3 inputs,
c     expr1, expr2, and expr3 based on the input expr0 less than,
c     equal to, or greater than zero
      if(expr0)1000,1010,1020
1000  fcnsw=expr1
      go to 1030
1010  fcnsw=expr2
      go to 1030
1020  fcnsw=expr3
1030  return
      end
      function deadsp(lobrk,hibrk,expr)
c...  deadspace routine is a unity gain block with a deadspace in
c     the input expt.
      real lobrk
      if(expr-hibrk)1000,1000,1020
1000  if(expr-lobrk)1030,1010,1010
1010  deadsp=0.0
      go to 1040
1020  deadsp=expr-hibrk
      go to 1040
1030  deadsp=expr-lobrk
1040  return
      end
      function funct(array, n, name, x)
c
c     linear interpolation of named array
c
      dimension array(2*n)
      character name *6
c     debug save
      save i,x1,x2,y1,y2
c
      if (n.le.0 .or. n.gt.30) then
        call page(4)
        write (6,1000) n, name
 1000   format(1h0, '*** N PARAMETER EQUAL TO ',i6,' FOR ',a6,
     1        'FUNCTION'//)
        call exit
      end if
c
      if (x.lt.array(1)) then
        write (6,1010) array(1), x, name
 1010   format(1h0, '*** LOWER RANGE VALUE ',f12.4,' RESET TO ',F12.4,
     1        ' FOR ',a6,' FUNCTION'//)
        array(1) = x
c        funct = array(2)
c        return
      else if (x.gt.array(2*n-1)) then
        write (6,1020)  array(2*n-1), x, name
 1020   format(1h0, '*** UPPER RANGE VALUE ',f12.4,' RESET TO ',F12.4,
     1        ' FOR ',a6,' FUNCTION'//)
        array(2*n-1)  = x
c        funct = array(2*n)
c        return
      end if
c
      do 100  i = 3, 2*n-1,2
      x2  = array(i)
      y2  = array(i+1)
      x1  = array(i-2)
      y1  = array(i-1)
c
      if (x.gt.x2) go to 100
      if (x.eq.x2) then
         funct  = y2
      else if (x.eq.x1) then
         funct  = y1
      else if (x.lt.x2) then
         funct  = y1 + (x-x1) * (y2-y1)/(x2-x1)
      end if
      return
c
 100  continue
      end
      subroutine setup
c
c     sets up for integration
c
      dimension ct(6), ifinv(10), jfinal(10)
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / minmax/ rmin(100), rmax(100), trmin(100), trmax(100)
c
      character *7  symb
      common / symbl/ symb(856)
c
      common / range/  irange(100)
c
      common / finish/ finval(10), ifinal(10), fam(10)
c
      common / flags/ iflag(50)
c
      common / integl/ c(73)
      common / y0intgl/ cy(73)
c
      equivalence (time, ct(1)), (finval(1), ifinv(1))
      equivalence (iend, iflag(9)), (istep, iflag(16))
      equivalence (ifirst,iflag(10)),(istart,iflag(18))
      equivalence (line, iflag(43)), (dtime, iflag(19))
      equivalence (ifin, iflag(33))
c     debug save
      save ci2,ci3,i,i2,i3,iopt,jfinal,k
c
c
      if (istep.ne.0) go to 1530
      if (krange.gt.0) then
c
         do 1050 i=1,krange
            trmin(i)=0.
            trmax(i)=0.
            rmin(i)=1.e30
 1050    rmax(i)=-1.e30
      end if
c
c...   ifirst set to 1 to initialize print and plot
c
      istep=1
      ifirst=1
      if (klock.le.0) tlast = 0.0
      line = 0
c
      if (fintim.le.0.0) then
         call page(4)
         write(6,1080)
 1080    format(45h0FINTIM IS ZERO. THIS CASE CANNOT BE EXECUTED)
         ifirst=5
         return
      end if
c
      if (delmin.le.0.0) delmin = fintim * 1.e-7
c
      if (kprint.gt.0) then
            if (prdel.le.0.0) then
               if (outdel.le.0.0) outdel = fintim/100.
               prdel = outdel
               h = prdel
            else
               if (outdel.le.0.0) then
                  outdel = prdel
                  h = prdel
               else
                  if (outdel.lt.prdel) then
                     outdel=prdel/aint(prdel/outdel+.5)
                     h=outdel
                  else if (outdel.eq.prdel) then
                     h = outdel
                  else if (outdel.gt.prdel) then
                     prdel=outdel/aint(outdel/prdel+.5)
                     h=prdel
                  end if
               end if
            end if
      else
         if ((kgraph + ngraph).le.0)  then
            if (delt.gt.0.0) then
               h = delt
            else
               h = fintim/100.
            end if
         else
            if (outdel.le.0.0) outdel = fintim/100.
            h = outdel
         end if
      end if
c
      fintim = h*aint((fintim-tlast)/h+.0001)+tlast
c
      if (delt.le.0.0) then
         delt = h/16.
c
      else
            if (delt.gt.h) then
               delt = h
            else if (delt.lt.h) then
               delt = h/aint((h/delt)+.9)
            end if
      end if
c
      line = line + 6
      write (6,1400) (symb(k),ct(k),k=2,6)
 1400 format (1h0,'TIMER VARIABLES'/(1x,a8,1h=,1pe11.4))

      if (klock.gt.0) go to 1420
c
c...   set variable for initial calculations
1410  continue
      if(krange .gt. 0) then
      do 1415 i=1,krange
      trmin(i)=0.
      trmax(i)=0.
      rmin(i)=1.e30
 1415 rmax(i)=-1.e30
      end if
      iopt=2
      time=0.0
      tlast=0.
c
c...   iend is flag for endd of simulation
 1420 iend = 0
      ifin=0
      nalarm=0
c       istart set to 0 for endd or contin condition
      istart=0
      dtime=0.
      tnext=tlast
c
c
      if (klock.gt.0) then
         keep = -1
         go to 1520
      end if
c
c...   set initial conditions for integrators
c
      if (nointg.gt.0) then
         do 1460 i=1,nointg
            c(i) = cy(i)
 1460    continue
      end if
c
c...   initialize derivative ssubroutine
      keep=0
c
      call initial
      iopt=3
c
      if (nointg.gt.0) then
         do 1490 i=1,nointg
            c(i) = cy(i)
 1490    continue
      end if
c   set initial conditions for memory blocks
c
      call update
c...   keep is set to identity point to store
      keep = 1
c
 1520 if (iopt.eq.2) call initial
      if (iopt.eq.3) call update
c
      keep=0
 1530 timex=time+0.5*delt
c
      if (nalarm.ne.0) then
         if (nalarm.gt.0) go to 1840
         call page(4)
         write(6,1550)
 1550 format(51h0***EXCEEDED MAXIMUM ITERATIONS ON IMPLICIT LOOP***)
         go to 1840
c
      end if
c
c ... compute min-max range for selected variables
c
      if (krange.gt.0) then
         do 1610 i = 1,krange
            i2 = irange(i)
            ci2 = cfind(i2)
            if (rmin(i).gt.ci2) then
               rmin(i) = ci2
               trmin(i) = time
            end if
            if (rmax(i).lt.ci2) then
               rmax(i) = ci2
               trmax(i) = time
            end if
 1610    continue
      end if
      if (timex.ge.fintim) go to 1840
c
c ... check for finish conditions other than time
c
      if (ifin.lt.0) go to 1850
      if (ifin.gt.0) go to 1730
      if (kfinis.le.0) then
         ifin = -1
      else
         do 1720 i = 1,kfinis
            i2 = ifinal(i)
            ci2 = cfind(i2)
            i3  = ifinv(i)
            if ((i3.gt.0) .and. (i3.le.nosymb))  then
               ci3 = cfind(i3)
               if (ci2.lt.ci3) then
                  jfinal(i) = 1
               else if (ci2.eq.ci3) then
                  go to 1820
               else if (ci2.gt.ci3) then
                  jfinal(i) = -1
               end if
            else
               if (ci2.lt.finval(i)) then
                  jfinal(i) = 1
               else if (ci2.eq.finval(i)) then
                  go to 1820
               else if (ci2.gt.finval(i)) then
                  jfinal(i) = -1
               end if
            end if
 1720    continue
         ifin = 1
      end if
      go to 1850
 1730 do 1810 i = 1,kfinis
      i2=ifinal(i)
      ci2 = cfind(i2)
      i3 = ifinv(i)
      if ((i3.le.0) .or. (i3.gt.nosymb))  then
         if (((jfinal(i).ge.0) .and. (ci2.ge.finval(i))) .or.
     1      ((jfinal(i).lt.0) .and. (ci2.le.finval(i)))) go to 1820
         go to 1810
      else
         ci3 = cfind(i3)
         if (((jfinal(i).ge.0) .and. (ci2.ge.ci3)) .or.
     1      ((jfinal(i).le.0) .and. (ci2.le.ci3))) go to 1820
         go to 1810
      end if
 1810 continue
      go to 1850
c
 1820 continue
      call page(3)
      write (6,1830)  symb(i2), cfind(i2)
 1830 format (1h0,'***SIMULATION HALTED***',a8,1h=,1pe12.4/)
 1840 iend=1
      go to 1860
c
 1850 if (timex.le.tnext) return
 1860 call outputs
      if (iend.le.0) return
c...   set variable for terminal calculations to determine rerun
      iopt = 4
      keep=0
c
c...   ifirst set to 3 for endd of case
      ifirst=3
      call outputs
c...   if keep is 2, rerun problem
      if (keep.eq.2) go to 1410
c...   iend set to 3 to indicate family
      if (iend.eq.3) go to 1410
c...   ifirst set to 4 to indicate endd of run.
      ifirst = 4
      call outputs
c...   reset terminal calculation variable
      iopt = 3
      return
      end subroutine setup
      subroutine integr
c
c     central integration routine
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / flags/ iflag(50)
c
      common / integl/ cy(73)
      common / yintgl/ chys(2,73)
      common / xintgl/ cx(73)
c
      equivalence(iflag(16),istep),(iflag(19),dtime),(iflag(40),istore)
      equivalence(iflag(18),istart),(iflag(10),ifirst)
c
      istep=0
 1000 call setup
c     write(*,*) "integ step ", time
      if (ifirst.ge.4) return
c
c...   trapezoidal integration
2000  do 2010 ii=1,nointg
         chys(1,ii) = cy(ii)
         chys(2,ii) = cx(ii)
 2010 continue
c
      dtime = dtime+1.
      time = dtime*delt+tlast
c
c...   predict y at time + delt
      do 2020 ii=1,nointg
         cy(ii) = delt * cx(ii) + cy(ii)
 2020 continue
c
c...   update inputs
      istart = 1
      keep = 0
      call update
c
c...   compute outputs of integrators, correct previous computation
c
      do 2030 ii=1,nointg
2030  cy(ii) = chys(1,ii)+.5*delt*(chys(2,ii)+cx(ii))
c
c...   keep is set to identify point to store
c
2040  keep = 1
      call update
      go to 1000
c
      end
      function adump(m,n,start)
c
      integer kx
      integer k1, k2
      dimension cc(50)
      character *6 dpdum(50), itypnt
      character *8 iblank, ititle(11), word
c
      character *60  title
      common / titles/ title(3)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / flags/ iflag(50)
c
      character *7  symb
      common / symbl/ symb(856)
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
c
      equivalence (idxfam, iflag(1)),  (nfam,  iflag(2))
      equivalence (line,  iflag(43)),  (line1, iflag(44))
      equivalence (idebug,iflag(45))
      equivalence  (cfix, kcfix)
c
      save    kx
      data    kx /0/
c     debug save
c      save cc,cfix,dpdum,i,iblank,igo,ii,ititle,itypnt,j,jj,k,kcfix
c     c,l,lfpt,ll,nb,word
c
c
      lfpt = 20
c
      if(time.le.0.0) then
         kx = n
         k1=nosymb+1
         k2=nosymb+lfpt
         idebug=1
         if (line.le.0) line = 50
         if (nfam.gt.1) word = symb(idxfam)
      end if
c
      if ((kx.le.0) .or. (time.lt.start)) then
         adump = start
         return
      end if
c
      kx = kx - 1
      igo=1
      ii=1
      j=5
c *** loop back to here  ****
 1120 continue
c
      jj=j-ii+1
      ll=1
      do 1130 l=ii,j
         dpdum(ll) = symb(l)
         cc(ll) = cfind(l)
         if (lfpt.gt.0) then
            do 1125 k = k1,k2
            if (dpdum(ll).eq.symb(k)) then
               cfix = cfind(l)
               cc(ll) = float(kcfix)
            end if
 1125       continue
         end if
1130  ll=ll+1
      go to(1140,1360),igo
c
 1140 if (line.le.48) then
         line = line + 1
      else
         if (nfam.gt.1) then
               call page(2)
               write (6,1180)  (title(i), i=1,3),
     1                       word, cfind(idxfam)
 1180          format (1h0,a60,/1x,a60,/1x,a60,2x,a8,1h=,1pe11.4)
         else
               call page(2)
               write (6,1230)  (title(i), i=1,3)
 1230          format (1h0,a60,/,(1x,a60))
         end if
      end if
c
      write(6,1320) keep
 1320 format (1h0,6x,'ADUMP OUTPUT   KEEP=',i2)
      line=line+2
      write(6,1340) (dpdum(nb),cc(nb),nb=1,jj)
 1340 format(1h ,1x,a8,1h=,1pe12.4,2x,a8,1h=,e12.4,3(4x,a8,1h=,e12.4))
c
      ii=6
      igo=2
 1350 if (ii.le.nosymb) then
         j = min0(j+4,nosymb)
         go to 1120
      else
         adump = start
         return
      end if
c
 1360 if (line.le.49) then
         line = line + 1
      else
         if (nfam.gt.1) then
            call page(2)
            write (6,1180)  (title(i), i=1,3)
     1            ,word ,cfind(idxfam)
         else
            call page(2)
            write (6,1230)  (title(i), i=1,3)
         end if
      end if
c
      write(6,1490) (dpdum(nb),cc(nb),nb=1,jj)
 1490 format(1h ,20x,4(4x,a8,1h=,1pe12.4))
      ii=ii+4
      go to 1350
      end
      function cfind(i)
c
c     arranges variables in common into single array
c
      common /  timer/ c1(6)
      common / integl/ c2(73)
      common / xintgl/ c3(73)
      common /y0intgl/ c4(73)
      common /inputs0/ c5(72)
      common           c6(653)
c
      common / flags/ iflag(50)
c
      equivalence  (line, iflag(43))
c
c
      if (i.le.0) go to 100
c
      if (i.le.6) then
         cfind = c1(i)
         return
      else if (i.le.77) then
         cfind = c2(i-6)
         return
      else if (i.le.148) then
         cfind = c3(i-77)
         return
      else if (i.le.219) then
         cfind = c4(i-148)
         return
      else if (i.le.291) then
         cfind = c5(i-219)
         return
      else if (i.le.944) then
         cfind = c6(i-291)
         return
      end if
c
 100  if (line.ge.55) call page(4)
      line = line + 5
      write (6,1000) i
 1000 format (1h0,i8,'VARIABLE OUT OF RANGE IN CFIND'///)
      cfind = 0.0
c
      return
      end
      subroutine page(i)
c
c     initializes or writes out page headings depending upon i
c        i=0 ===> initialization
c        i=1 ===> input  report page heading
c        i=2 ===> debug  report page heading
c        i=3 ===> output report page heading
c        i=4 ===> error  report page heading
c        i=5 ===> printer-plot  page heading
c
      integer pager
      character *13 title
      character *10 xdate,date
      common / flags/ iflag(50)
c
      equivalence (line,iflag(43)),(line1,iflag(44))
      save pager, xdate, nprob
c
      data    nprob/ 0/
c     debug save
c      save date,title
c
      line = line1
      if (i.eq.0) then
c
c     initialize
c
         nprob = nprob + 1
         pager = 0
         xdate = fdate()
      else
c
c        increment page # and assign appropriate title
c        then write out headings
c
         pager = pager + 1
         if (i.eq.1) title = 'INPUT  REPORT'
         if (i.eq.2) title = 'DEBUG  REPORT'
         if (i.eq.3) title = 'OUTPUT REPORT'
         if (i.eq.4) title = 'ERROR  REPORT'
         if (i.eq.5) title = 'PRINTER-PLOT '
         write (6,100) xdate,pager
         write(6,110) title, nprob
      end if
c
 100  format (1h1,30hGENERAL DYNAMICS CORPORATION  ,21x,
     1   29hCENTRAL DATA SYSTEMS CENTER  ,22x,a10,6h PAGE ,i3)
 110  format (1h ,19hFORT WORTH DIVISION,32x,16hF-16 HAVE BOUNCE,
     1        2x,a13,24x,8hPROB.NO.,i2)
      return
      end
      subroutine plotr
c
c     produces printer-plots
c
      dimension xout(50),outv(10)
      character *7 word,var,xname(4),fname,lxname,lyname
      character *1 iout(52)
c  required common
c
c     kplot       - number of prtplt variables
c     ngraph      - number of print plots
c     kgraph      - number of prepar variables
c     noplot(50)  - table containing number of variables to be read for
c                   each print plot
c     xaxis(50)   - lower bound of each variable to be print plotted
c     yaxis(50)   - upper bound of each variable to be print plotted
c     indxpp(50)  - indx in c array of each variable to be print plotted
c     pname(100)  - locations of variables requiring max/min tests
c     glabel(10)  - 66-char label from label card
c     xout(50)    - buffer used to store values for output
c     indexg(50)  - index in c array of variables appering on plot input
c     nointg      - number of integrators for problem
c     nosymb      - nubber of symbols in symbol table
c     symb(856)     - output names
c
      common / axis/ xaxis(10), yaxis(10), noplot(10)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      character *7  symb
      common / symbl/ symb(856)
c
      common / index/ indexg(50), indexp(50), indxpp(50)
c
      character *7  pname, hdng
      common / names/ pname(100), hdng(50)
c
      character *60  glabel
      common / label/ glabel(10)
c     debug save
c      save fkdiv,fname,i1,i2,i3,i4,idx,ii,ind,iout,ipage,j,jj,jk,k,kdiv
c     +,kk,kline,l,lines,lxname,lyname,novar,outv,qhigh,qlow,r,val,var
c     +,word,xname,xout
c
c
c
c   rewind input file
c
      rewind 14
      fkdiv=50.0
      lines=1000
      kdiv=52
      kk=1
      index = 1
      do 1420 i = 1,ngraph
      if (noplot(i).lt.0) then
         novar = -noplot(i)
         i4=11*kk-10
         i3=i4+10
         kk=kk+1
      else
         novar = noplot(i)
      end if
c
      ind = index + novar -1
      jj=0
      do 1030 k=index,ind
         j = indxpp(k)
         jj = jj+1
1030  xname(jj) = symb(j)
      fname=xname(1)
c
c  coordinate graph points
c
      if(yaxis(i).ne.xaxis(i)) then
         if(xaxis(i)+.001.eq.0) then
            call rang1(fname,qlow,r)
            qhigh = yaxis(i)
         else
            if(yaxis(i)+.001.eq.0) then
               call rang1(fname,r,qhigh)
               qlow=xaxis(i)
            else
               qlow=xaxis(i)
               qhigh=yaxis(i)
            end if
         end if
      else
         call rang1(fname,qlow,qhigh)
      end if
c
      read (14)   var,val
 1120 kline=1000
      ipage=0
 1130 read (14,end=1410) time, (xout(k), k=1,kplot)
c
c  check for endd condition
c
      jk=0
      idx=index
      do 1150 k=1,novar
         jk=jk+1
         outv(jk)=xout(idx)
1150  idx=idx+1
      kline=kline+1
      if(kline.gt.lines) then
         kline=0
         word = symb(1)
         call page(5)
         write(6,1170) glabel(i),word,fname
 1170 format(1h ,///,1h ,30x,a60,//,1h ,24x,7hMINIMUM,13x,a7,8h VERSUS
     1       ,2x,a7,7x,7hMAXIMUM)
c...   test for parameter family
         write (6,1190) qlow,qhigh
 1190    format (24x,1pe12.4,37x,e12.4)
         write (6,1230) word,(xname(k),k=1,jj)
 1230    format(2x,a7,7x,a7,5x,1hI,48x,1hI,7x,a7,6x,a7,5x,a7)
      end if
      iout(1) = ' '
      r=(outv(1)-qlow)/(qhigh-qlow)
      if (r.ge.1.0) then
         if (r.eq.1.0) then
            iout(kdiv-1) = '+'
            iout(kdiv) = ' '
            i2 = kdiv - 2
         else
            iout(kdiv) =  '*'
            i2 = kdiv - 1
         end if
         do 1330 ii = 2,i2
 1330    iout(ii) = '-'
      else
         if (r.lt.0.0) then
            iout(1) = '*'
            i1 = 2
            l = 1
         else
            l = r*fkdiv + 2.0
            iout(l) = '+'
            i1 = l + 1
            i2 = l - 1
         end if
         do 1310 ii = i1,kdiv
         iout(ii) = ' '
 1310    continue
         if (l.gt.2) then
            do 1331 ii = 2,i2
 1331       iout(ii) = '-'
         end if
      end if
c
      if(jk.le.1) then
         write(6,1360) time,outv(1),(iout(l),l=1,52)
 1360    format(1x,1pe12.4,2x,e12.4,2x,52a1)
      else
         write(6,1380)time,outv(1),(iout(l),l=1,52),(outv(l),l=2,jk)
 1380    format (1x,1pe12.4,2x,e12.4,2x,52a1,2x,3e12.4)
      end if
      go to 1130
c
1410  rewind 14
      index=index+novar
1420  continue
      call page(5)
      write(6,1430)
 1430 format (1h )
      return
      end
      subroutine outputs
c
c     prepares output report
c
      dimension  ibuf(50), xbuf(50), xout(50)
c
      character *8 iblank, ibcd(2), ititle(2), word
      character *8  ira, buf(50)
c
      common / index/ indexg(50), indexp(50), indxpp(50)
c
      character *7  pname, hdng
      common / names/ pname(100), hdng(50)
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      character *60  title
      common / titles/ title(3)
c
      common / flags/ iflag(50)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      character *7  symb
      common / symbl/ symb(856)
c
      common / minmax/ rmin(100), rmax(100), trmin(100), trmax(100)
c
      common / range/  irange(100)
c
      common / finish/ finval(10), ifinal(10), fam(10)
c      save jtem, dpt, dpl
c      save buf,cf1,dpl,dpt,i,i2,ibcd,iblank,ibuf,iconf,ira,ititle,ityp,j
c     +,jtem,k,kline,kpage,nlines,npts,stime,val,word,xbuf,xout
c
      equivalence  (idxfam, iflag(1)), (nfam, iflag(2))
      equivalence  (kfam, iflag(3)), (ends, iflag(4))
      equivalence  (iend, iflag(9)), (ifirst, iflag(10))
      equivalence  (iplot, iflag(11)), (iprint,iflag(12))
      equivalence  (line, iflag(43)), (line1, iflag(44))
c
c...   ifirst=1 to initialize plot and print
c...   ifirst=2 for normal output time
c...   ifirst=3 for terminal conditions
c...   ifirst=4 to indicate endd of run
c
      cf1 = cfind(1)
      go to(1000,1230,1860,2030,1220),ifirst
c
c ... ifirst = 1 ..............................
 1000 iblank='        '
      ibcd(1)='        '
      ibcd(2)='BEGIN   '
      val = 0.0
      kpage = 0
c
c ... set output control variable, jtem.
      jtem=1
      if ((kgraph + kplot).gt.0)  jtem = 2
      if (kprint.gt.0) jtem = jtem + 2
      if (nfam.gt.1)  word = symb(idxfam)
      line = 60
c
      nlines = (kprint + 7)/4
      kline = 55 - nlines
c
c...   write first label record on plot + prepar data sets
c
      if (kgraph.gt.0) then
c		 open(unit=13, file='label_on_plot_and_data_sets13.txt')
         if (nfam.gt.1) then
            write (13)   word, cfind(idxfam)
         else
			write(13) iblank, val
         end if
      end if
c
      if (kplot.gt.0)  then
         if (nfam.le.1) then
            write (14)  iblank, val
         else
            write (14)   word, cfind(idxfam)
         end if
      end if
c
c ... ifirst = 5 ..............................
 1220 ifirst=2
      tprint=tlast
      tplot=tlast
      dpt=0.
      dpl=0.
c
c ... ifirst = 2 ..............................
 1230 iplot=1
      iprint=1
      if (kgraph.gt.0) then
         if (timex.ge.tplot) then
            iplot = 2
            dpl = dpl + 1
            time = tplot
            tplot = dpl*outdel + tlast
            do 1280 i = 1,kgraph
               k = indexg(i)
               xout(i) = cfind(k)
 1280       continue
c			open(unit=13, file='cf1_xout_kgraph.txt', status='unknown')
			write(13) cf1, (xout(i), i=1, kgraph)
         else
            if (iend.ne.0) then
               do 1281 i = 1,kgraph
                  k = indexg(i)
                  xout(i) = cfind(k)
 1281          continue
            end if
         end if
      end if
c
      if (kprint.gt.0) then
         if (timex.ge.tprint) then
            iprint = 2
            dpt = dpt + 1
            time = tprint
            tprint = dpt*prdel + tlast
         else
            if (iend.eq.0) go to 1590
         end if
      else
         go to 1590
      end if
c
      if (line.lt.kline .and. kpage.eq.1) go to 1520
      kpage = 1
c
      if (nfam.gt.1) then
            call page(3)
            write (6,1370)  (title(i), i=1,3),
     1                       word, cfind(idxfam)
 1370       format (1h0,a60,/1x,a60,/1x,a60,2x,a8,1h=,1pe11.4)
      else
            call page(3)
            write (6,1420)  (title(i), i=1,3)
 1420       format (1h0,a60,/,(1x,a60))
      end if
      line = line + 3
c
      if (kprint.le.8)  then
            line = line + 1
            write (6,1510)  symb(1), (hdng(i), i=1,kprint)
 1510       format(1h0,2x,a8,8(4x,a8))
      end if
C
1520  do 1530 i=1,kprint
         k=indexp(i)
         xout(i) = cfind(k)
 1530 continue
      if (kprint.ge.9)  then
         write (6,1580)  symb(1),cf1, (hdng(i), xout(i), i=1,kprint)
 1580    format (1h0,1x,a8,1h=,1pe12.4,2x,a8,1h=,e12.4,
     1             3(4x,a8,1h=,e12.4)/(25x,a8,1h=,e12.4,
     1             4x,a8,1h=,e12.4,4x,a8,1h=,e12.4,
     1             4x,a8,1h=,e12.4))
         line = line + nlines
      else
         write (6,1560) cf1, (xout(i), i=1,kprint)
 1560    format (1pe12.4,8e12.4)
         line = line + 1
      end if
c
 1590 if (kplot.gt.0)  then
         if (iplot.eq.1)  then
            if (timex.ge.tplot) then
               iplot=2
               dpl=dpl+1
               time=tplot
               tplot=dpl*outdel+tlast
            else
               if (iend.eq.0) go to 1670
            end if
         end if
         do 1650 i=1,kplot
            k=indxpp(i)
 1650    xout(i) = cfind(k)
         write (14)  cf1, (xout(i), i=1,kplot)
      end if
c
      if (iend.lt.0) return
      if (iend.gt.0) go to 1730
 1670 go to(1680,1690,1700,1710),jtem
c
c ... jtem = 1
 1680 tnext=fintim
      return
c
c ... jtem = 2
 1690 tnext=tplot
      return
c
c ... jtem = 3
 1700 tnext=tprint
      return
c
c ... jtem = 4
 1710 tnext=amin1(tprint,tplot)
      return
c
c...   reset line for next print out
 1730 call page(3)
      if (krange.gt.0) then
         do 1750 i=1,krange
            if (irange(i).eq.1) go to 1760
 1750    continue
 1760    write(6,1770) rmin(i),rmax(i)
 1770    format(1h ,10x,16hPROBLEM DURATION, 1pe11.4,4h TO ,e11.4)
         line = line + 2
         write (6,1780) symb(1),symb(1)
 1780    format (1h0/9h VARIABLE,6x,7hMINIMUM,8x,a8,3x,7hMAXIMUM,6x,a8)
         do 1810 i=1,krange
            i2=irange(i)
            if (i2.eq.1)  go to 1810
            if (line.gt.50) call page(3)
            line = line + 1
            write (6,1800) symb(i2),rmin(i),trmin(i),rmax(i),trmax(i)
            write (2,1800) symb(i2),rmin(i),trmin(i),rmax(i),trmax(i)
 1800       format (2x,a8,3x,1pe11.4,3(2x,e11.4))
 1810    continue
      end if
c
c...   write ends on data sets
c
      if (kplot.gt.0)  endfile 14
      return
c
c ... test for rerun due to terminal conditions here
c ... ifirst = 3 ..............................
 1860 if (nfam.le.1)  return
      if (klock.gt.0) then
         if (line.gt.50) call page(3)
         line = line + 1
         write(6,1970)
 1970    format(46h0ONLY LAST VALUE OF FAMILY USED FOR CONTIN RUN)
         return
      else
         if (kfam.ge.nfam)  return
      call plotr
      rewind 14
      kpage = 0
      end if
c
      kfam=kfam+1
      call cset(idxfam, fam(kfam))
c
c...  iend set=3 to indicate family
      iend=3
c...  reset ifirst for normal output
      ifirst=5
      if (kgraph.gt.0)  write (13)  word, cfind(idxfam)
c
      if (kplot.le.0)  return
      write (14) word, cfind(idxfam)
      return
c
c ... ifirst = 4 ..............................
c ... endd of run.  complete data sets.
 2030 if (kgraph.gt.0) then
         endfile 13
         rewind 13
c ...    write names of variables being prepared
         do 2070 i=1,kgraph
            k = indexg(i)
            buf(i) = symb(k)
 2070    continue
c ...    write control record
         npts = kgraph + 1
         write (15)  iconf, npts, symb(1), (buf(i), i=1,kgraph),
     1               (title(i), i=1,3)
c ...    write ranges and times reached for variables
         do 2100 i=1,kgraph
            k=indexg(i)
            do 2080 j=1,krange
               if (k.eq.irange(j)) go to 2090
 2080       continue
 2090       ibuf(i) = j
            xbuf(  i)=rmin(j)
 2100    continue
         ityp = 20
         write (15) ityp, cf1, (xbuf(i), i=1,kgraph)
         do 2110 i=1,kgraph
            k=ibuf(  i)
            xbuf(i) = rmax(k)
 2110    continue
         write (15) ityp, cf1, (xbuf(i), i=1,kgraph)
         do 2120 i=1,kgraph
            k = ibuf(i)
            xbuf(i) = trmin(k)
 2120    continue
         write (15) ityp, cf1, (xbuf(i), i=1,kgraph)
         do 2130 i=1,kgraph
            k = ibuf(i)
            xbuf(i) = trmax(k)
 2130    continue
         write (15) ityp, cf1, (xbuf(i), i=1,kgraph)
c ...    copy output onto final data set
         read (13)   word,val
 2150    read (13,end=2160) stime, (xout(i), i=1,kgraph)
         ityp = 10
         write (15) ityp, stime, (xout(i), i=1,kgraph)
         if (stime.lt.cf1)  go to 2150
         read (13, end=2160) word, val
         ityp = 99
         write (15) ityp, stime, (xout(i), i=1,kgraph)
         ityp = 10
         go to 2150
      end if
c
 2160 rewind 13
      tlast=time
      ktitle=-ktitle
c
      return
      end
      function ylocal (vector,npoint,i,xlocal)
c
c     ylocal does the linear interpolation between the
c     data points defined in ssubroutine elevat's data statements
c
c
c     vector is the corresponding data obtained from the data statements
c                                                      in routine elevat
c     npoint is the number of data points in vector
c     i is the current location of the pointer in vector
c
      dimension vector(*)
c     debug save
c     save xp1,xp2,yp1,yp2
c
      ylocal = 0.0
 10   i = i + 1
      if (i.gt.npoint .or. i.le.1)  then
         i = 1
         return
      end if
c
      xp2 = vector (i*2-1)
      xp1 = vector (i*2-3)
c
      if (xp2.ge.xlocal .and. xp1.le.xlocal)  then
         yp2 = vector (i*2)
         yp1 = vector (i*2-2)
         ylocal = (xlocal -xp1) * (yp2 -yp1)/(xp2 -xp1)  + yp1
         return
      else if (xp1.gt.xlocal) then
         i = i - 2
      end if
      go to 10
c
      end
      subroutine profile
c
c  gear displacement due to profile.
c
c   dynamic segment of model
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      common  / functs/  bumtal(30) ,srampl(30)  ,fritab(30)
     1,bumtab(30) ,sramp(30) ,cltab(30) ,cmtab(30) ,cdtab(30)
     1,fantab(30)  ,knttab(30)  ,fcntab(30)  ,slptab(30)
     1,kmttab(30)  ,fcmtab(30)  ,famtab(30)  ,znttab(30)
     1,zmttab(30)  ,santab(30)  ,samtab(30)  ,esfvpc(16)
     1,vpcesf(16), nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c
c
c     nose gear location.
      ndx = xdist/foot
c
c     main gear location.
      mdx = ndx-dntm/foot
      call settime(mdx)
c
c     left main gear elevation
      bumpml = bump(mdx)
c
c     left main gear drag coefficient from ramp slopes
      rafril = slump(mdx)
c
      if (iasym.ne.1) then
c        right main gear elevation
         bumpmr = bumpml
c        drag coefficients resulting from ramp slopes.
         rafrir = rafril
c        nose gear elevation
         bumpn = bump(ndx)
c        nose gear drag coefficient from ramp slopes
         rafrin = slump(ndx)
      else
         bumpn  = 0.0
         bumpmr = 0.0
         rafrir = 0.0
         rafrin = 0.0
      end if
c
c
      return
      end
      subroutine main()
c
      common / index/ indexg(50), indexp(50), indxpp(50)
c
      common / minmax/ rmin(100), rmax(100), trmin(100), trmax(100)
c
      common / range/  irange(100)
c
      common / finish/ finval(10), ifinal(10), fam(10)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / flags/ iflag(50)
c
      common /  integl/ c1(73)
      common /  xintgl/ c2(73)
      common / y0intgl/ c3(73)
      common /  yintgl/ c4(2,73)
      common /  timer/ c5(6)
      common /inputs0/  c7(72)
      common           c6(653)

c      equivalence(iflag(16),istep)
c     debug save
c     save i,k,npt1
c
      do 100 i =1,653
         if (i.le.6) c5(i) = 0.0
         if (i.le.72) c7(i) = 0.0
         if (i.le.73) then
            c1(i) = 0.0
            c2(i) = 0.0
            c3(i) = 0.0
            c4(1,i) = 0.0
            c4(2,i) = 0.0
         end if
         c6(i) = 0.0
 100  continue
c
      call page(0)
c
c
c
      do 200 i = 1,50
         iflag(i)  = 0
         indexg(i) = 0
         indexp(i) = 0
         indxpp(i) = 0
 200  continue
c
      do 250 i = 1,100
         rmin(i)  = 0.0
         rmax(i)  = 0.0
         trmin(i) = 0.0
         trmax(i) = 0.0
         irange(i)= 0
 250  continue
c
      call initsys
c
      klock   = 0
      kprint  = 0
      kplot   = 0
      krange  = 0
      kfinis  = 0
      ktitle  = 0
      kgraph  = 0
      ngraph  = 0
c
      npt1    = nosymb + 20
      kpt1 = npt1 + 1
      k = 1
c
 300  call inputs(k)
c      istep = 0
      call integr
      if (kplot.gt.0)  call plotr
c
      write (6,1000)
 1000 format (1h0, 'END OF PROBLEM')
      k = 2
      call page(0)
      go to 300
c
      end
      subroutine inputs(kk)
c
      character *7 repeat(10), fname
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common /smtail/ismtail,smcltab(30),smcmtab(30),nsmcltb
     *,nsmcmtb,smclh,smcmh
c
      common /speedbr/   cdsbrnd ,cdsbr0
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common / xintgl/  zz0021  ,zz0024
     1,zz0027  ,zz0030  ,zz0033  ,zz0036  ,pii     ,delhd   ,thendd
     1,thmldd  ,thmrdd  ,zbnadd  ,zz0147  ,xbnadd  ,zz0148  ,zbmldd
     1,zz0149  ,xbmldd  ,zz0150  ,zbmrdd  ,zz0151  ,xbmrdd  ,zz0152
     1,qddt(2,20)
     1,deldd    ,zz0173   ,vipsd
     1,zz0174  ,thedd   ,zz0175  ,phidd   ,zz0176  ,zesfr1  ,zesfr9
c
      common / y0intgl/  al0     ,zz0022
     1,thedb0  ,an0     ,resp0   ,zz0039  ,piic    ,delh0   ,thend0
     1,themd0  ,zz0177  ,zbnad0  ,zbna0   ,xbnad0  ,xbna0   ,zbmad0
     1,zbma0   ,xbmad0  ,xbma0   ,zz0178  ,zz0179  ,zz0180  ,zz0181
     1,qd0(2,20)        ,deld0   ,del0    ,vips0
     1,x0      ,thed0   ,the0    ,zz0144  ,zz0146  ,zesfr10 ,zesfr90
c
      character *7  ppname(50)
      character *60  glabel
      common / label/ glabel(10)
c
      common / axis/ xaxis(10), yaxis(10), noplot(10)
c
      character *60  title
      common / titles/ title(3)
c
      common  / functs/  bumtal(30) ,srampl(30)  ,fritab(30)
     1,bumtab(30) ,sramp(30) ,cltab(30) ,cmtab(30) ,cdtab(30)
     1,fantab(30)  ,knttab(30)  ,fcntab(30)  ,slptab(30)
     1,kmttab(30)  ,fcmtab(30)  ,famtab(30)  ,znttab(30)
     1,zmttab(30)  ,santab(30)  ,samtab(30)  ,esfvpc(16)
     1,vpcesf(16), nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
c
      common / range/  irange(100)
c
      character *7  pname, hdng
      common / names/ pname(100), hdng(50)
c
      character *7  symb
      common / symbl/ symb(856)
c
      common / index/ indexg(50), indexp(50), indxpp(50)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common / finish/ finval(10), ifinal(10), fam(10)
c
      common /setime/ itime,findx,toffset
c
      common / flags/ iflag(50)
c
      common  / mats/ nmats, rwl, rwdx, xlmat(10), rlgt(10),
     1                rhgt(10), ulgt(10), uhgt(10), clgt(10),
     1                sag(10), xmat(10),
     1                rwstart,irunway,conbump(90),npnbump,rwshift
      common / repairs/ ibump, idmat, repair(3001)
      common /cleara/ fspta,wlpta,dipta
c
      common / pilot/ time1,c2,c3,thetamx,c4,c5
     *,deltime,contime,conndx,convfps,cbtfri,cthstar,ctnvfps
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c
      equivalence  (idxfam, iflag(1)),  (nfam, iflag(2))
      equivalence  (kfam, iflag(3))
c     debug save
      save cbrfri,fname,i,iconf,ij,n1,nconf,ni
     +,nrepeat,ppname
c
c **********************************************************************
c
c
      namelist /timer/ time, delt, delmin, fintim, prdel, outdel
     1,itime   ,findx   ,toffset
c
      namelist / const/  fsng    ,wlng    ,fsmg0   ,wlmg    ,an0
     1         ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd      ,dax     ,mm      ,imt     ,mn      ,int     ,wlth
c
      namelist / param/  rolfri  ,vref    ,iasym   ,irigid
     1,brfri   ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm  ,mdbasn
     1,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip      ,landg
     1,alpha   ,vsink   ,vtaxi   ,weight       ,ithe    ,fscg    ,wlcg
     1,clh     ,cmh     ,delth0  ,sas     ,th      ,ndb     ,tdb
     1,fspta   ,wlpta   ,kvtaxi  ,time1   ,c2      ,c3      ,thetamx
     1,c4      ,c5      ,deltime ,contime ,conndx  ,convfps ,cbrfri
     1,cthstar ,cdsbrnd ,cdsbr0  ,smclh   ,smcmh   ,ismtail
     1,ctnvfps
c
      namelist / incon/  thed0   ,xf0     ,gcom
c
      namelist / table/  omeg    ,gm      ,ce      ,crsa    ,csc
c
      namelist / functs/  bumtal ,srampl  ,fritab  ,bumtab  ,sramp
     1,cltab   ,cmtab   ,cdtab   ,fantab  ,knttab  ,fcntab  ,slptab
     1,kmttab  ,fcmtab  ,famtab  ,znttab  ,zmttab  ,santab  ,samtab
     1,esfvpc  ,vpcesf  ,nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
     1,smcltab ,smcmtab ,nsmcltb ,nsmcmtb
c
      namelist / mats/ ibump, idmat, nmats, rwl, rwdx, xlmat,
     1                 rlgt, rhgt, ulgt, uhgt, clgt, sag, xmat,
     1                 rwstart,irunway,conbump,npnbump,rwshift
c
      namelist /finish/ finval, ifinal, fam, nfam, fname, kfinis
c
      namelist / control/ kprint, kgraph, kplot, krange, ngraph, hdng
c
      namelist /  prplot/ ppname, glabel, xaxis, yaxis, noplot,
     1                    ngraph, kplot
c
      namelist / config/ weight, ithe, fscg, wlcg, iphi, nsm, nam,
     1             omeg, gm, ce, crsa, csc, nr, iconf
c
      namelist /  run/ repeat, nrepeat, pass, nconf, ibump, idmat
c
c *********************************************************************
c
      if(kk .eq. 1) iconf = -1
      read (9,1000,end=300)  (title(i), i=1,3)
 1000 format (a60)
      call page(1)
      write (6,2000)  (title(i), i=1,3)
 2000 format (1h0,5x,a60,/,(6x,a60))
      read (9,run, end=300)
c
c
      go to (5, 510), kk
c
 5    read (4,timer,end=300)
      read(4,const)
      read(4,param)
      read(4,incon)
      read (4,finish)
      if (nfam.gt.1)  then
         kfam = 1
         do 30  i = 1, nosymb
            if (fname.eq.symb(i)) then
               idxfam = i
               go to 40
            end if
 30      continue
         write (6,2100)  fname
         nfam = 1
 40      continue
      end if
c
      read(4,table)
      read(4,functs)
      read (4,mats)
c
      read (4,control)
c
      if (kprint.gt.0) then
         read (4,1100)  (hdng(i), i=1,kprint)
 1100    format (10a7)
         do 100  j=1,kprint
         do 90   i=1,nosymb
            if (hdng(j).eq.symb(i)) then
               indexp(j) = i
               go to 100
            end if
 90      continue
            write (6,2100) hdng(j)
 2100       format(5x,a7,' VARIABLE NOT FOUND IN SYMBOL TABLE',//)
            indexp(j) = 1
 100     continue
      end if
c
      if (kgraph.gt.0) then
         read (4,1100)  (pname(i), i=1,kgraph)
         do 120  j = 1,kgraph
         do 110  i = 1,nosymb
            if (pname(j).eq.symb(i))  then
               indexg(j) = i
               go to 120
            end if
 110     continue
         write (6,2100)  pname(j)
         indexg(j) = 1
 120     continue
      end if
c
      if (kplot.gt.0 .or. ngraph.gt.0) then
         read (4,prplot)
         if (kplot.eq.0)  kplot = ngraph
         if (ngraph.eq.0)  ngraph = kplot
         do 20 j = 1,kplot
            pname(j+kgraph) = ppname(j)
         do 10 i = 1,nosymb
            if (ppname(j).eq.symb(i))  then
               indxpp(j) = i
               go to 20
            end if
 10      continue
            write (6,2100) ppname(j)
            indxpp(j) = 1
 20      continue
      end if
c
      n1 = kgraph + kplot
c
      if (krange.gt.0) read (4,1100)  (pname(n1+i), i=1,krange)
      krange = krange + kgraph + kplot + 1
      if (krange.gt.0)  then
c ...    time added to irange variable table
         irange(1) = 1
         do 140 j = 1,krange-1
         do 130 i = 1,nosymb
            if (pname(j).eq.symb(i)) then
               irange(j+1) = i
               go to 140
            end if
 130     continue
         write (6,2100) pname(j)
         irange(j+1) = 1
 140     continue
      end if
c
  55     read (4, config, end=600)
c
      if ((nconf+1).lt.0)  go to 600
 510  if (nconf.lt.iconf)  then
         rewind 4
         iconf = -1
         go to 5
      else if (nconf.eq.iconf)  then
         go to 520
      end if
      if(nconf .gt. iconf) then
         go to 55
      end if
      go to 510
c		
 520  if (nrepeat.gt.0)  then
         do 530 i = 1,nrepeat
         if (repeat(i).eq.'TIMER')  then
            read (9,timer)
            go to 530
         else if (repeat(i).eq.'CONST') then
            read (9,const)
            go to 530
         else if (repeat(i).eq.'PARAM') then
            read (9,param)
            go to 530
         else if (repeat(i).eq.'INCON') then
            read (9,incon)
            go to 530
         else if (repeat(i).eq.'FINISH') then
            read (9,finish)
            if (nfam.gt.1)  then
         kfam = 1
         do 230  ij = 1, nosymb
            if (fname.eq.symb(ij)) then
               idxfam = ij
               go to 240
            end if
 230     continue
         write (6,2100)  fname
         nfam = 1
 240     continue
      end if
c
            go to 530
         else if (repeat(i).eq.'TABLE') then
            read (9,table)
            go to 530
         else if (repeat(i).eq.'FUNCTS') then
            read (9,functs)
            go to 530
         else if (repeat(i).eq.'MATS') then
            read (9,mats)
            go to 530
         else if (repeat(i).eq.'CONTROL') then
            read (9,control)
      if (kprint.gt.0) then
         do 101  j=1,kprint
         do 91   ni=1,nosymb
            if (hdng(j).eq.symb(ni)) then
               indexp(j) = ni
               go to 101
            end if
 91      continue
            write (6,2100) hdng(j)
            indexp(j) = 1
 101     continue
      end if
            go to 530
         else if (repeat(i).eq.'PRPLOT') then
            read (9,prplot)
            go to 530
         else if (repeat(i).eq.'CONFIG') then
            read (9,config)
            go to 530
         end if
         write (6,2300)  repeat(i)
 2300    format (1h0, a7,' NOT FOUND IN NAMELIST'/)
         call exit
 530     continue
      end if
c
      return
c
c
 300  write (6,3000)
 3000 format (1h0,' END OF JOB')
      call exit
c
 600  write (6,2200)  nconf
 2200 format (1h0,i6,' NOT FOUND IN CONFIGURATION LIST')
      call exit
      end
      function bump(xlocal)
c
c     functionn bump determines the height of any point (xlocal)
c     along the runway by linear interpolation
c     xlocal is in feet units
c
c     repair is the profile of the runway
c
c
c     common block mats contains runway profile information
c
      common  / mats/ nmats, rwl, rwdx, xlmat(10), rlgt(10),
     1                rhgt(10), ulgt(10), uhgt(10), clgt(10),
     1                sag(10), xmat(10),
     1                rwstart,irunway,conbump(90),npnbump,rwshift
c
      common / repairs/ ibump, idmat, repair(3001)
c
c
c     j is saved for a subsequent slope call
c
      save j
c     debug save
c      save ratio
c      bump = bumph(xlocal)
c      return
      if ((xlocal.gt.0.).and.(xlocal.lt.rwl)) then
c
c        determine the height of the point
c
         j = ifix(xlocal/rwdx)
         ratio = (xlocal-rwdx*j)/rwdx
         bump = ratio*(repair(j+2)-repair(j+1)) + repair(j+1)
c
      else
c
c        for all values of xlocal  greater than
c        the length of the runway, bump is set to repair(j+1)
c
         bump = repair(j+1)
      end if
      if(xlocal .le. 0.)bump=0.
      return
      entry slump(xlocal)
c
c     entry point slump
c
c     slump returns the slope of the runway at the point xlocal
c     -----> note:  the value of j is saved in functionn bump
c
      slump = bumps(xlocal)
      return
      if ((xlocal.gt.0.).and.(xlocal.lt.rwl)) then
c
c        calculate the slope at the point
c
         slump = (repair(j+2) - repair(j+1))/rwdx /12.0
c
      else
c
c        slump set to 0 if xlocal > runway length or xlocal <= 0
c
         slump = 0.0
      end if
      return
      end
      subroutine elevat
c
c     elevat generates the runway's profile for have bounce
c
      common  / mats/ nmats, rwl, rwdx, xlmat(10), rlgt(10),
     1                rhgt(10), ulgt(10), uhgt(10), clgt(10),
     1                sag(10), xmat(10),
     1                rwstart,irunway,conbump(90),npnbump,rwshift
c
      common / repairs/ ibump, idmat, repair(3001)
c
c
      integer point
      dimension con01b(12),con02b(20),con03b(12),con04b(20)
      dimension con05b(10),con06b(14),con07b(12),con08b(20)
c     debug save
      save b,con01b,con02b,con03b,con04b,con05b,con06b,con07b,con08b
     +,cos0,i,id,j,kount,n,npnt01,npnt02,npnt03,npnt04,npnt05,
     +npnt06,npnt07,npnt08,pi,point,r,rwlx,xlocal
      data point,npnt01,npnt02,npnt03,npnt04/1,6,10,6,10/
      data npnt05,npnt06,npnt07,npnt08/5,7,6,10/
      data con01b/-500.,0.,15.,0.,18.5,1.5,88.5,
     $1.5,92.,0.,100000.,0./
      data con02b/-500.,0.,15.,0.,18.5,1.5,88.5,
     $1.5,92.,0.,162.,0.,165.5,1.5,235.5,1.5,
     $239.,0.,100000.,0./
      data con03b/-500.,0.,15.,0.,22.5,3.,88.5,3.,
     $92.,0.,100000.,0./
      data con04b/-500.,0.,15.,0.,22.5,3.,40.5,
     $3.,44.5,1.5,62.5,1.5,66.5,3.,84.5,3.,
     $92.,0.,100000.,0./
      data con05b/-500.,0.,4.,0.,4.5,-1.5,5.,0.,10000.,0./
      data con06b/-500.,0.,4.,0.,4.5,-1.5,5.,0.,
     $5.5,-1.5,6.,0.,10000.,0./
      data con07b/-500.,0.,4.,0.,4.5,-1.5,5.5,-1.5,
     $6.,0.,100000.,0./
      data con08b/-500.,0.,4.,0.,4.5,-1.5,5.5,-1.5,
     $6.,0.,7.,0.,7.5,-1.5,8.5,-1.5,9.,0.,100000.,0./
c
c     common block mats contains the runway profile generated by elevat
c
c     common block mats is also used for the input data for elevat
c     nmats    - number of mats
c     rwl      - length of runway used
c     rwdx     - increment length
c
c     repair is the profile of the repairs done to the runway
c
c     ibump = 1 the repair with the mats defind with "mats"
c           = 2 the repair defind with config 1
c           = 3 the repair defind with config 2
c           = 4 the repair defind with config 3
c           = 5 the repair defind with config 4
c           = 6 the repair defind with config 5
c           = 7 the repair defind with config 6
c           = 8 the repair defind with config 7
c           = 9 the repair defind with config 8
c           =10 the repair defind with conbump
c
c     kount is the functional length of the array repair
c
      kount = ifix(rwl/rwdx) + 1
c
c     check for bad input data
c
      if(kount .lt. 5 .or. kount.ge.3000)  go to 190
c
      call runway(rwdx,rwl,rwstart,irunway,repair)
c
      go to(1,400,420,440,460,600,620,640,660,680)ibump
c
      go to 180
c
c *****************************************************************
c     xlmat- mat length
c     rlgt- ramp length
c     rhgt- ramp height
c     ulgt- upheaval length
c     uhgt  - upheaval height
c     clgt- crater length
c     sag- sag
c     xmat- distance from starting location to edge of mat
c
c     check for bad input data
c
 1    do 5 i = 1,nmats
         if(2.*rlgt(i)+clgt(i).gt.xlmat(i))go to 150
         if(2.*ulgt(i) .gt. rwl)go to 160
         if((xmat(i)+xlmat(i)) .gt. rwl)  go to 170
 5    continue
c
c
      pi=3.1416
      id = idmat
c
c
c     the following loop determines the values of the repair for the mat
c     configuration defined by "mats"
c
      do 2000 i = 1,nmats
c
         do 100 j=1,kount
            xlocal = (j-1)*rwlx+rwshift
c           skip calculations if not over mat
 20         if (xlocal .lt. (xmat(i)+xlmat(i)) .and.
     $             xlocal .gt. xmat(i))            then
c           calculate repair profile
c              r - the distance from crater center
               r = abs(xlocal-xmat(i) - xlmat(i)/2.)
               if (r .gt. clgt(i)/2.-ulgt(i)) go to 40
               b = -sag(i)/2.
c
c              over the crater hole
               cos0 = cos(pi*(1.-r/(clgt(i)/2.-ulgt(i))))
               repair(j) = b*(1.-cos0)+uhgt(i)+rhgt(i) + repair(j)
               go to 100
c
c              on upheaval
 40            if(r .gt. clgt(i)/2.) go to 50
               repair(j) = uhgt(i)/ulgt(i)* (clgt(i)/2.-r)+ rhgt(i)
     $                      + repair(j)
               go to 100
c
c              on level mat
 50            if(r .gt. xlmat(i)/2.-rlgt(i)) go to 60
               repair(j) = rhgt(i) + repair(j)
               go to 100
c
c              on ramp
 60            repair(j) = rhgt(i)/rlgt(i)*(xlmat(i)/2.-r) + repair(j)
c
            end if
 100     continue
c
c
 2000 continue
c
       return
c
c
c     the following loops will determine the values of repair for the
c     corresponding configuration.  ylocal is an external functionn that
c     does the linear interpolation between the data points defined in
c     this routine's data statements (above).
c
c
c     configuration 1 - single mat
c
 400  do 410 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 410  repair(i)=ylocal(con01b,npnt01,point,xlocal)+repair(i)
      return
c
c     config 2 - double mat
c
 420  do 430 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 430  repair(i)=ylocal(con02b,npnt02,point,xlocal)+repair(i)
      return
c
c
c     config 3 - single bump
c
 440  do 450 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 450  repair(i)=ylocal(con03b,npnt03,point,xlocal)+repair(i)
      return
c
c
c     config 4 - double bump
c
 460  do 470 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 470  repair(i)=ylocal(con04b,npnt04,point,xlocal)+repair(i)
      return
c
c
c     configuration 5 - single spall no. 1
c
 600  do 610 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 610  repair(i)=ylocal(con05b,npnt05,point,xlocal)+repair(i)
      return
c
c     config 6 - double spall no. 1
c
 620  do 630 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 630  repair(i)=ylocal(con06b,npnt06,point,xlocal)+repair(i)
      return
c
c
c     config 7 - single spall no. 2
c
 640  do 650 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 650  repair(i)=ylocal(con07b,npnt07,point,xlocal)+repair(i)
      return
c
c
c     config 8 - double spall no. 2
c
 660  do 670 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 670  repair(i)=ylocal(con08b,npnt08,point,xlocal)+repair(i)
      return
c
c
c     config defind by conbump
c
 680  do 690 i=1,kount
         xlocal =(i-1)*rwdx+rwshift
 690  repair(i)=ylocal(conbump,npnbump,point,xlocal)+repair(i)
      return
c ******************************************************************
c
c     write statement for error in input data
c
c
 150  write(6,155)
 155  format(/,18h INPUT DATA ERROR.,/,
     $51h ERROR MOST LIKELY WAS RAMP LENGtH OR CRATER LENGTH,
     $15h OR MAT LENGTH.)
      go to 99
c
 160  write(6,165)
 165  format(/,18h INPUT DATA ERROR.,/,
     $55h ERROR MOST LIKELY WAS UPHEAVAL LENGTH OR CRATER LENGTH)
      go to 99
c
 170  write(6,175)
 175  format(/,18h INPUT DATA ERROR.,/,
     $41h ERROR LENGTH OF RUNWAY USED WAS TO SMALL)
      go to 99
 180  write(6,185)
 185  format(/,18h INPUT DATA ERROR.,/,
     $24h ERROR IBUMP IS TO LARGE)
      go to 99
c
 190  write(6,195)
 195  format(/,18h INPUT DATA ERROR.,/,
     $54h INCORRECT LENGTH OF RUNWAY AND INCREMENT LENGTH GIVEN)
c
c
 99   write(6,200)nmats,rwl,rwdx
 200  format(/,11h INPUT DATA,/,
     $20h NUMBER AT MATS    -,f6.0,/,
     $20h LENGTH OF RUNWAY  -,f9.3,/,
     $20h INCREMENT LENGTH  -,f9.3,/)
c
      do 205,n=1,nmats
 205  write(6,210)n,xlmat(n), rlgt(n), rhgt(n), ulgt(n), uhgt(n),
     1            clgt(n), sag(n), xmat(n)
 210  format(/,17h(FOR MAT NUMBER -,i2,2h ),
     $16h MAT LENGTH    -,f9.2,5h FEET,/,
     $16h RAMP LENGHT   -,f9.2,5h FEET,/,
     $16h RAMP HEIGHT   -,f9.2,7h INCHES,/,
     $18h UPHEAVAL LENGTH -,f9.2,5h FEET,/,
     $18h UPHEAVAL HEIGHT -,f9.2,7h INCHES,/,
     $18h CRATER LENGTH   -,f9.2,5h FEET,/,
     $18h SAG             -,f9.2,7h INCHES,/,
     $35h STARTING LOCATION TO EDGE OF MAT -,f9.2,5h FEET)
      write(6,500)
 500  format(1h ,'BAD INPUT DATA-TERMINATED IN SUBROUTINE HEIGHT')
      call exit
c
 1000 return
      end
      subroutine respons
c
c test to see if response is to be calculated
c
      common / xintgl/  zz0021  ,zz0024
     1,zz0027  ,zz0030  ,zz0033  ,zz0036  ,pii     ,delhd   ,thendd
     1,thmldd  ,thmrdd  ,zbnadd  ,zz0147  ,xbnadd  ,zz0148  ,zbmldd
     1,zz0149  ,xbmldd  ,zz0150  ,zbmrdd  ,zz0151  ,xbmrdd  ,zz0152
     1,qddt(2,20)
     1,deldd    ,zz0173   ,vipsd
     1,zz0174  ,thedd   ,zz0175  ,phidd   ,zz0176  ,zesfr1  ,zesfr9
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common /cleara/ fspta,wlpta,dipta
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common    zz0019  ,zz0025  ,zz0028  ,zz0031  ,zz0034  ,zz0035
     1         ,dntm    ,h0      ,stv     ,stl
     1,dbv     ,dbl     ,ndn     ,xds     ,zds     ,lsl     ,bet0
     1,lsu     ,gam0    ,cosgam  ,singam  ,cosbet  ,sinbet  ,lsubmg
     1,cosbmg  ,ntm     ,zz0017  ,waplg   ,dn0     ,dm0     ,lnsu
     1,lmsu    ,dth     ,dar     ,dn      ,lns     ,lms     ,fdnt
     1,fdmt    ,cos0    ,sin0    ,omcos0  ,vtd     ,qs0     ,cl0
     1,cm0     ,lift0   ,momen0  ,fnt     ,fngp    ,fngv    ,fmt
     1,fmg     ,znt     ,zmt     ,fstal   ,fstnl   ,fnmg    ,nstrk
     1,mstrk   ,snn     ,dxbma   ,zstf    ,zdbf    ,snv     ,snl
     1,deeone  ,lata    ,th1     ,hdel    ,th2     ,rn      ,rm
      common nthe
     1,pitch   ,mass    ,axtdb   ,fdba    ,fstau   ,fstnu   ,fstv0
     1,fstl0   ,fdbv0   ,fdbl0   ,fnmg0   ,fmdba   ,fmdbv0  ,fmsv0
     1,mgsw    ,ndx     ,zz0018  ,bumpn   ,mdx     ,bumpml  ,bumpmr
     1,rafrin  ,rafrir  ,rafril  ,costhe  ,sinthe  ,omcos   ,zna
     1,znad    ,height  ,zntd1   ,zntd    ,xnad    ,ntslip  ,fnslip
     1,zmal    ,zmtl    ,rml     ,fmtl    ,xmald   ,mtslpl  ,fmslpl
     1,fdmtl   ,brtorl  ,zmar    ,zmtr    ,rmr     ,fmtr    ,zmard
     1,mtslpr  ,fmslpr  ,fdmtr   ,brtorr  ,j       ,l       ,dld3
     1,dld5    ,dld4    ,dld6    ,dldd3   ,dldd5   ,dldd4   ,dldd6
      common sna
     1,snvd    ,snld    ,snad    ,snnd    ,ff1     ,ff2     ,fan
     1,fcn     ,snada   ,fst1    ,fst2    ,fstv    ,fstl    ,fdbv
     1,fdbl    ,fngl    ,sml     ,lmsl    ,dm      ,smdl    ,smdla
     1,mstrkl  ,fcml    ,faml    ,fm1l    ,fm2l    ,fmgl    ,dxbmal
     1,dxbmld  ,fnmgl   ,fmgr    ,fnmgr   ,lmsr    ,mstrkr  ,smr
     1,smdr    ,smdra   ,fcmr    ,famr    ,fm1r    ,fm2r    ,dxbmar
     1,dxbmrd  ,fmdbvr  ,fmdbvl  ,vfps    ,alp     ,cl      ,cm
     1,qs      ,delh    ,alp2    ,alpf    ,theddd  ,deldd1  ,delddg
     1,thedb   ,prin    ,resp    ,resp2   ,dhcom1  ,dhcom2  ,alpk
      common pi5
     1,pil     ,pk      ,pl      ,pia     ,pig     ,piii    ,dlhd
     1,lift    ,cd1     ,cdrag   ,drag    ,k       ,dawd    ,aerom
     1,ilc     ,r1      ,r2      ,r3      ,r4      ,r5      ,zbug
     1,delpta,vknots,kvtaxi,thrust,bumpa,adx
      common ce      ( 100)
      common dlf     (   7)
      common qdd     (  20)
      common qdot    (  20)
      common q       (  20)
      common gm      (  20)
      common gomeg   (  20)
      common omeg    (  20)
      common omsq    (  20)
      common dld     (   5)
      common adld    (   5)
      common dldd    (   5)
      common adldd   (   5)
      common r       (   5)
      common crsa    ( 120)
      common   csc(22)
c
      real            ksem
     1,knmg    ,ka      ,kd      ,mm      ,imt     ,mn      ,int
     1,iphi    ,mustr   ,mdbarm  ,mdbasn  ,mdn     ,ithe    ,knttab
     1,kmttab  ,ndn     ,lsl     ,lsu     ,lsubmg  ,lnsu    ,lmsu
     1,lns     ,lms     ,lift0   ,momen0  ,nstrk   ,mstrk   ,lata
     1,nthe    ,mass    ,mgsw    ,ndx     ,mdx     ,ntslip  ,mtslpl
     1,mtslpr  ,lmsl    ,mstrkl  ,lmsr    ,mstrkr  ,lift    ,insw
     1,limit
c     debug save
      save i,kmttab,knttab
c
      if(keep.ne.1)go to 51
      if(time.eq.0.)go to 58
      if(ilc-ip)210,58,58
58    ilc=0
c augment the modal accelerations with the rigid accelerations.
59    qdd(ntm+1)=deldd*costhe-vipsd*sinthe
      qdd(ntm+2)=thedd
      qdd(ntm+3)=vipsd*costhe+deldd*sinthe
      qdd(ntm+4)=phidd
c     center line tank or ecm pod clearance
      adx=ndx+(-fspta+fsng)/12.
      bumpa=bump(adx)
      delpta=del-(fspta-fscg)*sinthe+(wlcg-wlpta)*omcos+dipta-bumpa
c
c calculate the incremental responses.
      do 203 i=1,nr
203   r(i)=0.
      do 204 i=1,nr
      l=ntm+4
      j=(i-1)*l
      do 204 k=1,l
204   r(i)=r(i)+crsa(j+k)*qdd(k)
      r1=r(1)
      r2=r(2)
      r3=r(3)
      r4=r(4)
      r5=r(5)
500   format(1p,8e14.5)
210   ilc=ilc+1
 51   zbug = adump (1, ndb, tdb)
      go to 39999
c  terminal segment of model
39999 continue
      return
      end
      subroutine runway(rwdx,rwl,rwstart,irunway,repair)
      dimension repair(3001)
c
c     the runway profile in tape1 starting at location
c     rwstart is superimposed onto the given repair
c
c     if irunway is equal to zero, the superimposition of
c     the runway does not occur
c
c     debug save
c      save i,iend,j,kcheck,kount,nkount,ratio
c     +,xafter,xbefore,xdel,xlocal,zero
c explicitly declare the temporal eof functionn as integer
      integer eof
      iend=0
      zero=0
      kount=ifix(rwl/rwdx)+1
      do 10 i=1,kount
10    repair(i)=0.
      if(irunway .eq. 0)return
      read(1,*)xdel
      nkount=ifix(rwstart/xdel)+1
      do 30 i=1,nkount
      read(1,*)xbefore
30    if(eof(1) .ne. 0)go to 80
      read(1,*)xafter
      if(eof(1) .ne. 0)go to 80
      do 70 j=1,kount
      if(iend .eq. 1)go to 60
      xlocal=rwstart+(j-1)*rwdx
      kcheck=ifix(xlocal/xdel)+1
40    if(nkount .eq. kcheck)go to 50
      xbefore=xafter
      nkount=nkount+1
      read(1,*)xafter
      if(eof(1) .ne. 0)go to 60
      go to 40
50    ratio=(xlocal-xdel*(nkount-1))/xdel
      repair(j)=ratio*(xafter-xbefore)+xbefore
      if(j .eq. 1)zero=repair(1)
      repair(j)=(repair(j)-zero)*12.
      go to 70
60    iend=1
      repair(j)=(xbefore-zero)*12.
      write(6,*)'ERROR BAD INPUT NKOUNT GT KCHECK'
70    continue
      return
80    write(6,*)'ERROR BAD INPUT OFF OF RUNWAY'
      return
      end
      subroutine control (mgsw)
c
c     pitch elevator stick control
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom
c
      common / integl/   alpl    ,thdw0
     1,thedb1  ,glag    ,resp1   ,zz0037  ,pi      ,dela    ,thend
     1,themld  ,themrd  ,zbnad   ,zbna    ,xbnad   ,xbna    ,zbmald
     1,zbmal   ,xbmald  ,xbmal   ,zbmard  ,zbmar   ,xbmard  ,xbmar
     1,qdott(2,20)
     1,deld    ,del     ,vips
     1,xdist   ,thed    ,the     ,phid    ,phi     ,esfr1   ,esfr9
c
      common / xintgl/  zz0021  ,zz0024
     1,zz0027  ,zz0030  ,zz0033  ,zz0036  ,pii     ,delhd   ,thendd
     1,thmldd  ,thmrdd  ,zbnadd  ,zz0147  ,xbnadd  ,zz0148  ,zbmldd
     1,zz0149  ,xbmldd  ,zz0150  ,zbmrdd  ,zz0151  ,xbmrdd  ,zz0152
     1,qddt(2,20)
     1,deldd    ,zz0173   ,vipsd
     1,zz0174  ,thedd   ,zz0175  ,phidd   ,zz0176  ,zesfr1  ,zesfr9
c
      common / y0intgl/  al0     ,zz0022
     1,thedb0  ,an0     ,resp0   ,zz0039  ,piic    ,delh0   ,thend0
     1,themd0  ,zz0177  ,zbnad0  ,zbna0   ,xbnad0  ,xbna0   ,zbmad0
     1,zbma0   ,xbmad0  ,xbma0   ,zz0178  ,zz0179  ,zz0180  ,zz0181
     1,qd0(2,20)        ,deld0   ,del0    ,vips0
     1,x0      ,thed0   ,the0    ,zz0144  ,zz0146  ,zesfr10 ,zesfr90
c
      common  / functs/  bumtal(30) ,srampl(30)  ,fritab(30)
     1,bumtab(30) ,sramp(30) ,cltab(30) ,cmtab(30) ,cdtab(30)
     1,fantab(30)  ,knttab(30)  ,fcntab(30)  ,slptab(30)
     1,kmttab(30)  ,fcmtab(30)  ,famtab(30)  ,znttab(30)
     1,zmttab(30)  ,santab(30)  ,samtab(30)  ,esfvpc(16)
     1,vpcesf(16),nvpcesf
     1,nslptab ,nsamtab ,nbumtal ,nsrampl ,nfritab ,nbumtab ,nsramp
     1,ncltab  ,ncmtab  ,ncdtab  ,nfantab ,nknttab ,nfcntab
     1,nkmttab ,nfcmtab ,nfamtab ,nznttab ,nzmttab ,nsantab ,nesfvpc
c
      common / pilot/ time1,c2,c3,thetamx,c4,c5,
     *deltime,contime,conndx,convfps,cbtfri,cthstar,ctnvfps

      common / userin/ esfin, brkin

c
c   landg = -6  taxi run, thstar is an functionn of velocity for take off.
c   landg = -5  taxi run, brfri and thstar are functions of velocity.
c   landg = -4  taxi run, brfri and thstar are functions of distance.
c   landg = -3  taxi run, brfri and thstar are functions of time.
c   landg =  2  landing run, pitch down esf command.
c   landg =  3  landing run, brfri is an functionn of time.
c   landg =  4  landing run, brfri is an functionn of distance.
c   landg =  5  landing run, brfri is an functionn of velocity.
c
c   for all other values of landg no compulation will occur.
c
c     the input constants time1, c2, c3, and thetamx are only used
c     for landg=2 - the pitch down esf(elevator stick force) command.
c
c     time1 - pitch rate speed up time
c
c     thetamx - limit pitch rate
c
c     c3 - gain for esf: recommended value c3 = 60.
c
c     c2 - pitch damping: recommended value c2 = -8.
c
c     deltime - transition time of brfri from 0 to cbrfri
c
c     contime - time braking begins ( sec )
c
c     conndx - distance braking begins ( ft )
c
c     convfps - velocity braking begins ( ft/sec )
c
c     ctnvfps - velocity aft stick begins (ft/sec)
c
c     cbrfri value of brfri to be used in simlation
c
c     c4 - gain related to difference in desired and actual pitch angle
c          for esf: recommended value c4 = 6.
c
c     c5 - gain related to difference in desired and actual pitch rate
c          for esf: recommended value c5 = 360.
c
c     thstar - desired pitch angle
c
      real mgsw,limit
c     debug save
c      save alp,c1,cbrfri,esf,esfr3,esfr4,esfr5,esfr6,esfr7,
c     +esfr8,switch,thdsta,thetad,thstar,when
c
c
      if(landg .eq. -6)go to 10
      if(landg .eq. -5)go to 10
      if(landg .eq. -4)go to 10
      if(landg .eq. -3)go to 10
      if(landg .eq. 2)go to 10
      if(landg .eq. 3)go to 10
      if(landg .eq. 4)go to 10
      if(landg .eq. 5)go to 10
      return
10    continue
c
      if(landg .eq. 2)then
c
      c1=thetamx/time1
c
c     desired pitch rate, thetad
c
      if(time .le. time1)thetad=c1*time
      if(time .gt. time1)thetad=thetamx
      if(thetad .lt. (c2*the))thetad=c2*the
c
c     elevator stick force - esf
c
      esf=c3*(thetad-thed)
      end if
c
c
c     landing run
c     landg = 3, brfri is an functionn of time
c     using contime
      if(landg .eq. 3)then
      degpr=57.2958
      thstar=13.
      if(time .eq. 0.)brfri=0.
      if(time.ge.contime.and.brfri.lt.cbrfri.and.keep.eq.0)
     *brfri=brfri+cbrfri*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
c
c
c     landing run
c     landg = 4, brfri is an functionn of distance
c     using conndx
c
      if(landg .eq. 4)then
      degpr=57.2958
      thstar=13.
      if(time .eq. 0.)brfri=0.
      if(xdist/12..ge.conndx.and.brfri.lt.cbrfri.and.keep.eq.0)
     *brfri=brfri+cbrfri*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
c
c
c     landing run
c     landg = 5, brfri is an functionn of velocity
c     using convfps
c
      if(landg .eq. 5)then
      degpr=57.2958
      thstar=13.
      if(time .eq. 0.)brfri=0.
      if(vips/12..le.convfps.and.brfri.lt.cbrfri.and.keep.eq.0)
     *brfri=brfri+cbrfri*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
c
c
c     taxi run
c     landg = -3, brfri and thstar are functions of time
c     using contime
c
      if(landg .eq. -3)then
      degpr=57.2958
      if(time .eq. 0.)thstar=0.
      if(time .eq. 0.)brfri=0.
      if(time.ge.contime.and.brfri.lt.cbrfri.and.keep.eq.0)
     *brfri=brfri+cbrfri*delt/deltime
      if(time.ge.contime.and.thstar.lt.cthstar.and.keep.eq.0)
     *thstar=thstar+cthstar*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
      if(landg .eq. -3 .and. time .lt. contime)return
c
c
c     taxi run
c     landg = -4, brfri and thstar are functions of distance
c     using conndx
c
      if(landg .eq. -4)then
      degpr=57.2958
      if(time .eq. 0.)thstar=0.
      if(time .eq. 0.)brfri=0.
      if(xdist/12..ge.conndx.and.brfri.lt.cbrfri.and.keep.eq.0)
     *brfri=brfri+cbrfri*delt/deltime
      if(xdist/12..ge.conndx.and.thstar.lt.cthstar.and.keep.eq.0)
     *thstar=thstar+cthstar*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
      if(landg .eq. -4 .and. xdist/12. .lt. conndx)return
c
c
c     taxi run
c     landg = -5, brfri and thstar are functions of velocity
c     using convfps and ctnvfps
c
      if(landg .eq. -5)then
      degpr=57.2958
      if(time .eq. 0.)thstar=0.
      if(time .eq. 0.)brfri=0.
      if(vips/12..le.convfps.and.brfri.lt.cbrfri.and.keep.eq.0)
     *brfri=brfri+cbrfri*delt/deltime
      if(vips/12..le.ctnvfps.and.thstar.lt.cthstar.and.keep.eq.0)
     *thstar=thstar+cthstar*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
      if(landg .eq. -5 .and. vips/12. .gt. ctnvfps)return
c
c
c
c     taxi run
c     landg = -6, thstar is an functions of velocity for take off.
c     using ctnvfps
c
      if(landg .eq. -6)then
      degpr=57.2958
      if(time .eq. 0.)thstar=0.
      if(vips/12..ge.ctnvfps.and.thstar.lt.cthstar.and.keep.eq.0)
     *thstar=thstar+cthstar*delt/deltime
      thdsta=0.
      alp=the*degpr
      if(vips .gt. 120.)
     *    alp=(-deld/vips+the)*degpr
      esf=c4*(thstar-alp)+c5*(thdsta-thed)+zesfr10/2.
      end if
      if(landg .eq. -6 .and. vips/12. .lt. ctnvfps)return

c      esf = esfin
c      brfri = brkin
c
c     ****************************************************************
c
c     pitch response from esf
c     for f-16 flight control system
c     block 15 aircraft
c
c     esfr1=realpl(0.,.016667,esf)
c     esfr1=intgrl(zesfr10,zesfr1)
      zesfr1=(esf-esfr1)/.016667
      esfr3=funct(esfvpc,nesfvpc,"ESFVPC",esfr1)
      esfr4=deadsp(0.,10000.,esfr3)
      esfr5=limit(-4.,0.,esfr4)
      esfr6=limit(0.,8.,esfr3)
      esfr7=esfr6+esfr5
c
c     switch = 1 in flight
c     switch = 2 on ground
c
      switch=2.
      if(mgsw .gt. 0.)switch=1.
      esfr8=esfr7*switch
c
c     esfr9=realpl(0.,.12048,esfr8)
c     esfr9=intgrl(zesfr90,zesfr9)
      zesfr9=(esfr8-esfr9)/.12048
      gcom=esfr9
      when=amod(time,outdel)
c     if(when .lt. delt/2. .and. keep .eq. 1)
c    *write(6,100)esf,esfr1,esfr3,esfr4,esfr5,esfr6,esfr7,esfr8,
c    *esfr9,zesfr9,the,thed,alp
c100  format(4x,1p,14e9.2)
      return
      entry contrli(mgsw)
      if(landg .eq. -6)go to 20
      if(landg .eq. -5)go to 20
      if(landg .eq. -4)go to 20
      if(landg .eq. -3)go to 20
      if(landg .eq. 2)go to 20
      if(landg .eq. 3)go to 20
      if(landg .eq. 4)go to 20
      if(landg .eq. 5)go to 20
      return
20    continue
      zesfr90=gcom
      switch=2.
      if(mgsw .gt. 0.)switch=1.
      esfr7=zesfr90/switch
      esfr3=funct(vpcesf,nvpcesf,"VPCESF",esfr7)
      zesfr10=esfr3
c     write(6,110)gcom,zesfr90,zesfr10
c110  format(4x,5hgcom ,8hzesfr90 ,8hzesfr10 ,1p,3e11.3)
      return
      end
      subroutine settime(mdx)
      common /timer/ time,delt,delmin,fintim,prdel,outdel
      common /setime/ itime,findx,toffset
c
c     ssubroutine settime resets fintim to the time
c     the aircraft travels the distance findx plus
c     plus an continuation time toffset
c
      real mdx
c     debug save
      save icheck
      if(itime .gt. 0)then
      if(time .eq. 0.)then
      fintim=10.
      icheck=0
      else if(mdx .ge. findx .and. icheck .eq. 0)then
      icheck=1
      fintim=time+toffset
      end if
      end if
      return
      end
      subroutine smtlin(cltab,cmtab,ncltab,ncmtab,clh,cmh)
c
c     initialization of cltab cmtab ncltab ncmtab clh cmh
c     to their block 10 small tail control system conterparts
c     smcltab smcmtab nsmcltb nsmcltb smclh smcmh
c
      common /smtail/ismtail,smcltab(30),smcmtab(30),nsmcltb
     *,nsmcmtb,smclh,smcmh
c
      dimension cltab(30),cmtab(30)
c
      if(ismtail .ne. 1)return
c
      clh=smclh
      cmh=smcmh
      ncltab=nsmcltb
      ncmtab=nsmcmtb
c
      do 10 i=1,12
      cltab(i)=smcltab(i)
10    cmtab(i)=smcmtab(i)
c
      return
      end 
	  integer function eof(f)
      integer f
      eof = 0
      return
      end
      subroutine intstep()
c
c     central integration routine
c
      common /timer/ time, delt, delmin, fintim, prdel, outdel
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / flags/ iflag(50)
c
      common / integl/ cy(73)
      common / yintgl/ chys(2,73)
      common / xintgl/ cx(73)
c
      equivalence(iflag(16),istep),(iflag(19),dtime),(iflag(40),istore)
      equivalence(iflag(18),istart),(iflag(10),ifirst)
c
 1000 call setup
c     write(*,*) "integ step ", time
      if (ifirst.ge.4) return
c
c...   trapezoidal integration
2000  do 2010 ii=1,nointg
         chys(1,ii) = cy(ii)
         chys(2,ii) = cx(ii)
 2010 continue
c
      dtime = dtime+1.
      time = dtime*delt+tlast
c
c...   predict y at time + delt
      do 2020 ii=1,nointg
         cy(ii) = delt * cx(ii) + cy(ii)
 2020 continue
c
c...   update inputs
      istart = 1
      keep = 0
      call update
c
c...   compute outputs of integrators, correct previous computation
c
      do 2030 ii=1,nointg
2030  cy(ii) = chys(1,ii)+.5*delt*(chys(2,ii)+cx(ii))
c
c...   keep is set to identify point to store
c
2040  keep = 1
      call update
      return
      end
      subroutine allinit()
c
      common / index/ indexg(50), indexp(50), indxpp(50)
c
      common / minmax/ rmin(100), rmax(100), trmin(100), trmax(100)
c
      common / range/  irange(100)
c
      common / finish/ finval(10), ifinal(10), fam(10)
c
      common / three/ nalarm, kpoint, kprint, tprint, kplot,tplot,
     1         kfinis,krange,klock,kpt1,tlast,ktitle,nosymb,nointg,
     1        h, keep, timex, tnext, ngraph, kgraph
c
      common / flags/ iflag(50)
c
      common /  integl/ c1(73)
      common /  xintgl/ c2(73)
      common / y0intgl/ c3(73)
      common /  yintgl/ c4(2,73)
      common /  timer/ c5(6)
      common /inputs0/  c7(72)
      common           c6(653)

      equivalence(iflag(16),istep)
c     debug save
      save i,k,npt1
c
      do 100 i =1,653
         if (i.le.6) c5(i) = 0.0
         if (i.le.72) c7(i) = 0.0
         if (i.le.73) then
            c1(i) = 0.0
            c2(i) = 0.0
            c3(i) = 0.0
            c4(1,i) = 0.0
            c4(2,i) = 0.0
         end if
         c6(i) = 0.0
 100  continue
c
      call page(0)
c
c
c
      do 200 i = 1,50
         iflag(i)  = 0
         indexg(i) = 0
         indexp(i) = 0
         indxpp(i) = 0
 200  continue
c
      do 250 i = 1,100
         rmin(i)  = 0.0
         rmax(i)  = 0.0
         trmin(i) = 0.0
         trmax(i) = 0.0
         irange(i)= 0
 250  continue
c
      call initsys
c
      klock   = 0
      kprint  = 0
      kplot   = 0
      krange  = 0
      kfinis  = 0
      ktitle  = 0
      kgraph  = 0
      ngraph  = 0
c
      npt1    = nosymb + 20
      kpt1 = npt1 + 1
      k = 1
c
 300  call inputs(k)
      istep = 0
      return
      end
      real function getstate(i)

      common /  integl/ c1(73)

      getstate = c1(i)
      return
      end
      subroutine setesf(esf)

      common / userin/ esfin, brkin

      esfin = esf

      end
      subroutine setbrk(brk)

      common / userin/ esfin, brkin

      brkin = brk

      end
      subroutine setth(thin)

      common /inputs0/   fsng    ,wlng    ,fsmg0
     1,wlmg    ,fsst    ,wlst    ,fsdb    ,wldb    ,dbfs    ,dbwl
     1,rm0     ,rn0     ,s       ,fs35c   ,cbar    ,degpr   ,one
     1,two     ,foot    ,g       ,dens    ,cmq     ,fam0    ,ksem
     1,csem    ,knmg    ,cnmg    ,fan0    ,ka      ,ca      ,kd
     1,cd,dax  ,mm      ,imt     ,mn      ,int     ,wlth    ,rolfri
     1,brfri   ,vref    ,iphi    ,arml    ,ctd     ,mustr   ,mdbarm
     1,mdbasn  ,mdn     ,gdamp   ,nsm     ,nam     ,nr      ,ip
     1,landg   ,iasym   ,irigid  ,alpha   ,vsink   ,vtaxi   ,weight
     1,ithe    ,fscg    ,wlcg    ,clh     ,cmh     ,delth0  ,sas
     1,th      ,ndb     ,tdb     ,xf0     ,gcom

      th = thin

      end
