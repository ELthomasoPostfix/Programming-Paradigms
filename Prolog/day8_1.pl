/* The following links, in addition to the official SWISH docs, proved useful:
 * 		Text Representation (i.e. the different string literal syntaxes, atoms, ...):
 * 			https://www.swi-prolog.org/pldoc/man?section=text-representation
 * 		Recursion depth counter (i.e. counting how deep a function recurses):
 * 			https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
 * */



% Check if the given character code corresponds to a left or right move.
% Note: the following character codes hold: 'L' == 76, 'R' == 82
is_left_move(CharCode)  :- char_code('L', LCode), CharCode == LCode.
is_right_move(CharCode) :- char_code('R', RCode), CharCode == RCode.


% Base case: zzz is the terminal node, and zzz can be reached from zzz in zero steps.
follow_path(zzz, _, 0).
% Recursive cases: find a path from the current node to the terminal node, by navigating the left and right nodes of the current node.
% Prevent infinite recursion: if left(CurrNode) or right(CurrNode) is a self loop, then backtrack.
% Provide the path length as output through the third parameter. The following SO post served as inspiration: https://stackoverflow.com/questions/47366029/prolog-recursion-length-count
follow_path(CurrNode, [], PathLength) :-
    next_moves([], ResetMoves), follow_path(CurrNode, ResetMoves, PartialPathLength),
    PathLength is PartialPathLength.
follow_path(CurrNode, [CurrMove|RemainingMoves], PathLength) :-
    is_left_move(CurrMove),
    left(CurrNode, LeftNode),
    CurrNode \= LeftNode,
    follow_path(LeftNode, RemainingMoves, PartialPathLength),
    PathLength is PartialPathLength + 1.
follow_path(CurrNode, [CurrMove|RemainingMoves], PathLength) :-
    is_right_move(CurrMove),
    right(CurrNode, RightNode),
    CurrNode \= RightNode,
    follow_path(RightNode, RemainingMoves, PartialPathLength),
    PathLength is PartialPathLength + 1.

% The initial L-R string of moves; wrap around if no more moves left.
% The initial string, represented by '< MOVE_PATTERN_TARGET >', should be
% substituted by its true value.
next_moves([], `LRLRLRLRRLRRRLRLRLRRRLLRRLRRLRRLLRRLRRLRLRRRLRRLLRRLRRRLRRLRRRLRRRLLLRRLLRLLRRRLLRRLRLLRLLRRRLLRRLRRLRRRLRRLRLRRLRRLRLLRLRRRLRLRRLRLLRRLRRRLRRLRLRRLLLRRLRRRLRRRLRRLRRRLRLRRLRRLRRRLRRLRRLRRLRRLRRRLLRRRLLLRRRLRRLRRRLLRRRLRRLRRLLLLLRRRLRLRRLRRLLRRLRRLRLRLRRRLRRRLRRLLLRRRR`).
% Pop the frontmost move from the 'string' of moves.
next_moves([_|Rest], Rest).

:- discontiguous right/2.
:- discontiguous left/2.

left(bpq, vxr).
right(bpq, tln).
left(nsk, frm).
right(nsk, gxv).
left(xvg, bbc).
right(xvg, sgf).
left(jtg, lvr).
right(jtg, mpk).
left(rnq, tmp).
right(rnq, tlt).
left(vvx, tvh).
right(vvx, gcm).
left(djh, mmh).
right(djh, llq).
left(lqr, hff).
right(lqr, fhp).
left(qfq, cft).
right(qfq, stf).
left(rjl, tfg).
right(rjl, ctr).
left(drb, gbq).
right(drb, nrb).
left(khk, xdb).
right(khk, dvq).
left(ncg, shp).
right(ncg, ltm).
left(ksd, kqt).
right(ksd, pdb).
left(mgl, ldg).
right(mgl, dcn).
left(kfx, hlg).
right(kfx, vgf).
left(kfg, jgb).
right(kfg, gvh).
left(jlv, cgx).
right(jlv, mqn).
left(ccb, jxl).
right(ccb, lhd).
left(csj, gqs).
right(csj, fhj).
left(vpx, rgl).
right(vpx, xmd).
left(chv, dhp).
right(chv, dkg).
left(lrv, mlc).
right(lrv, hqq).
left(bpj, qjp).
right(bpj, vlr).
left(vff, pss).
right(vff, nfd).
left(fcm, jbg).
right(fcm, nrk).
left(gbq, jrq).
right(gbq, tlq).
left(vlm, krh).
right(vlm, bmq).
left(gxs, tks).
right(gxs, tqz).
left(nnm, ldg).
right(nnm, dcn).
left(qvn, tmp).
right(qvn, tlt).
left(jcr, glg).
right(jcr, kfv).
left(mfm, mxn).
right(mfm, pmm).
left(glg, jsk).
right(glg, hht).
left(lgh, rxd).
right(lgh, ndn).
left(crh, mcd).
right(crh, dsj).
left(fdd, xtr).
right(fdd, qss).
left(qqx, xps).
right(qqx, xps).
left(xgf, lpl).
right(xgf, pmr).
left(dfg, ftr).
right(dfg, jtk).
left(hhv, khk).
right(hhv, qdj).
left(ldx, gbp).
right(ldx, rfp).
left(jjd, snc).
right(jjd, cst).
left(cgg, vxr).
right(cgg, tln).
left(vqb, lbm).
right(vqb, rbd).
left(qrt, djh).
right(qrt, bkc).
left(kfp, svn).
right(kfp, scg).
left(bkc, llq).
right(bkc, mmh).
left(kjf, bgb).
right(kjf, ctl).
left(vdl, xfx).
right(vdl, qgq).
left(jnn, csx).
right(jnn, jqx).
left(ldv, ttq).
right(ldv, cqr).
left(sjd, qnm).
right(sjd, nsk).
left(rpd, xfq).
right(rpd, sks).
left(bmr, mrh).
right(bmr, rsm).
left(cnk, qqk).
right(cnk, dhq).
left(mld, mnc).
right(mld, bqt).
left(pqs, vln).
right(pqs, ljc).
left(hkk, rqp).
right(hkk, vgd).
left(qcs, kvd).
right(qcs, xpb).
left(kvd, qrt).
right(kvd, tvp).
left(jmk, blv).
right(jmk, pqs).
left(cjf, kpd).
right(cjf, grj).
left(dtg, tcx).
right(dtg, lvm).
left(dqg, gqq).
right(dqg, cqk).
left(vnn, qnf).
right(vnn, cdc).
left(shv, llv).
right(shv, qnq).
left(hqq, vnn).
right(hqq, xpg).
left(lvm, lqh).
right(lvm, nvm).
left(vfp, vpx).
right(vfp, mrg).
left(sbg, pxl).
right(sbg, lnc).
left(pxl, pcq).
right(pxl, rvk).
left(cxh, sbl).
right(cxh, skv).
left(htm, ppf).
right(htm, rct).
left(gbp, jmx).
right(gbp, jmx).
left(qmn, bsj).
right(qmn, nbp).
left(ddj, lrf).
right(ddj, nhv).
left(qms, gnt).
right(qms, dmh).
left(qdg, csf).
right(qdg, nsb).
left(vgr, qms).
right(vgr, bxx).
left(mcb, ssq).
right(mcb, knf).
left(jvl, pcc).
right(jvl, mqx).
left(rcq, pqp).
right(rcq, cmp).
left(nqk, kxg).
right(nqk, dqr).
left(cft, qhq).
right(cft, xkh).
left(cmp, nps).
right(cmp, dxn).
left(kkv, bpj).
right(kkv, qbg).
left(sgf, ctp).
right(sgf, kkq).
left(bkl, rcr).
right(bkl, nfs).
left(jhn, cgn).
right(jhn, shv).
left(glb, hmc).
right(glb, nfq).
left(qtg, frf).
right(qtg, lmn).
left(nfs, lqx).
right(nfs, kcl).
left(rnk, qcb).
right(rnk, brj).
left(klx, vst).
right(klx, svr).
left(smk, jsj).
right(smk, qtg).
left(kqj, fns).
right(kqj, npd).
left(cqr, jnc).
right(cqr, bkp).
left(mcp, hlm).
right(mcp, slc).
left(hmp, qrn).
right(hmp, bdn).
left(dqj, cnk).
right(dqj, gsr).
left(xfd, hmv).
right(xfd, xpr).
left(bdb, pqs).
right(bdb, blv).
left(bxg, chv).
right(bxg, vvm).
left(cgn, qnq).
right(cgn, llv).
left(jdb, fcs).
right(jdb, fbx).
left(xfc, mjt).
right(xfc, qml).
left(pmr, sjd).
right(pmr, tgd).
left(gvp, sbs).
right(gvp, kjf).
left(xps, dqj).
right(xps, dqj).
left(kcf, hct).
right(kcf, dbs).
left(krh, gdd).
right(krh, klx).
left(fdt, lhd).
right(fdt, jxl).
left(vlh, vjp).
right(vlh, ncg).
left(jnc, hpt).
right(jnc, vdq).
left(jmx, mvj).
right(jmx, mvj).
left(rkn, cjf).
right(rkn, cjf).
left(pfs, lhb).
right(pfs, fdd).
left(qxg, rlp).
right(qxg, dxm).
left(ctl, jvx).
right(ctl, mvn).
left(ctp, jng).
right(ctp, vfp).
left(fbk, dql).
right(fbk, kkx).
left(xbh, xqk).
right(xbh, lmv).
left(rxh, dhm).
right(rxh, dhm).
left(xnn, npm).
right(xnn, gjf).
left(sks, xjp).
right(sks, mql).
left(rnc, fkf).
right(rnc, rgs).
left(hnt, ntv).
right(hnt, rsg).
left(rcr, lqx).
right(rcr, kcl).
left(lmn, pdt).
right(lmn, qxg).
left(dgk, qqx).
right(dgk, rck).
left(qgs, gkl).
right(qgs, phd).
left(jsj, lmn).
right(jsj, frf).
left(jng, mrg).
right(jng, vpx).
left(khf, mhr).
right(khf, rdm).
left(mqx, mks).
right(mqx, lrq).
left(bxn, lmv).
right(bxn, xqk).
left(rct, drk).
right(rct, qkj).
left(ngl, mmq).
right(ngl, jnn).
left(lxl, rch).
right(lxl, tjf).
left(mvn, nln).
right(mvn, ccc).
left(kgq, dml).
right(kgq, vct).
left(svn, dbr).
right(svn, jjd).
left(bkq, xkj).
right(bkq, jnm).
left(lca, cnk).
right(lca, gsr).
left(hhb, hmk).
right(hhb, hjp).
left(nhb, bvp).
right(nhb, jbl).
left(sxd, skc).
right(sxd, rdj).
left(tcx, lqh).
right(tcx, nvm).
left(cst, qtc).
right(cst, kvk).
left(qrn, rpd).
right(qrn, qqf).
left(mxs, dqg).
right(mxs, njc).
left(dpn, lrv).
right(dpn, dgh).
left(ljj, tsp).
right(ljj, skl).
left(lrf, xfd).
right(lrf, ckq).
left(rsg, qdl).
right(rsg, gcd).
left(jbc, slc).
right(jbc, hlm).
left(rvk, rbm).
right(rvk, jrp).
left(xqg, dgh).
right(xqg, lrv).
left(qdj, dvq).
right(qdj, xdb).
left(smt, bhn).
right(smt, ktm).
left(tpf, sfv).
right(tpf, bvd).
left(lgs, rsq).
right(lgs, jcr).
left(dmb, crh).
right(dmb, fqd).
left(lvn, qms).
right(lvn, bxx).
left(mvj, tks).
right(mvj, tks).
left(vxr, rbp).
right(vxr, rcq).
left(csx, tnj).
right(csx, djg).
left(tvh, rxh).
right(tvh, rxh).
left(dxm, lpf).
right(dxm, lss).
left(hct, kfx).
right(hct, cmc).
left(lhb, qss).
right(lhb, xtr).
left(xnq, dqj).
right(xnq, xcz).
left(nhk, pdq).
right(nhk, fln).
left(dkt, qcl).
right(dkt, qcl).
left(bvv, jnn).
right(bvv, mmq).
left(xhp, ddj).
right(xhp, sqn).
left(lmk, ltk).
right(lmk, mpv).
left(nnh, nkm).
right(nnh, kgq).
left(cfg, dpn).
right(cfg, xqg).
left(blm, cnq).
right(blm, kjx).
left(jks, mfx).
right(jks, vvf).
left(dhm, xvl).
right(dhm, xvl).
left(drk, lxl).
right(drk, pch).
left(tlc, tpq).
right(tlc, frj).
left(kpf, shl).
right(kpf, nqh).
left(ccq, jmk).
right(ccq, bdb).
left(lqk, cgg).
right(lqk, bpq).
left(shl, fqg).
right(shl, xls).
left(mjv, pnf).
right(mjv, pnf).
left(tnj, ksd).
right(tnj, dhx).
left(qdl, gjh).
right(qdl, cmx).
left(bvf, hnt).
right(bvf, cpm).
left(ksb, bph).
right(ksb, mxs).
left(ffb, sfv).
right(ffb, bvd).
left(pmm, lsv).
right(pmm, fcm).
left(jrq, ldv).
right(jrq, frp).
left(vmh, bpj).
right(vmh, qbg).
left(kpd, kbg).
right(kpd, pfq).
left(grt, kkx).
right(grt, dql).
left(njf, dcg).
right(njf, mnk).
left(hmc, vvx).
right(hmc, vcd).
left(ntv, qdl).
right(ntv, gcd).
left(gvh, lrr).
right(gvh, hrc).
left(ncl, lvn).
right(ncl, vgr).
left(jrv, qbq).
right(jrv, hjf).
left(frf, pdt).
right(frf, qxg).
left(nxp, cgn).
right(nxp, shv).
left(jpr, gbq).
right(jpr, nrb).
left(rfp, jmx).
right(rfp, vsh).
left(qcb, gbt).
right(qcb, bhr).
left(mbt, npd).
right(mbt, fns).
left(dth, vgr).
right(dth, lvn).
left(grj, kbg).
right(grj, pfq).
left(nps, fvb).
right(nps, fhl).
left(vvm, dkg).
right(vvm, dhp).
left(trt, kkv).
right(trt, vmh).
left(jsp, nrt).
right(jsp, nkc).
left(vlr, rjl).
right(vlr, nqr).
left(ppz, qkg).
right(ppz, hcb).
left(vhv, drc).
right(vhv, kmb).
left(bgg, ddj).
right(bgg, sqn).
left(csf, mld).
right(csf, btc).
left(qqf, xfq).
right(qqf, sks).
left(ftm, cnq).
right(ftm, kjx).
left(kkj, rpc).
right(kkj, rrk).
left(qtv, hfn).
right(qtv, fxj).
left(rrl, jdh).
right(rrl, tmt).
left(ltt, hhv).
right(ltt, ktd).
left(tbp, snf).
right(tbp, rmh).
left(mcd, fmm).
right(mcd, dfg).
left(pqq, xfc).
right(pqq, dss).
left(hmv, phh).
right(hmv, mxl).
left(qcl, lcn).
right(qcl, lcn).
left(mvx, ssq).
right(mvx, knf).
left(dxn, fvb).
right(dxn, fhl).
left(skr, rcr).
right(skr, nfs).
left(bvp, bmr).
right(bvp, jvj).
left(nbp, ltt).
right(nbp, dbk).
left(hpt, jks).
right(hpt, gns).
left(lpl, sjd).
right(lpl, tgd).
left(rsq, glg).
right(rsq, kfv).
left(nbq, bpl).
right(nbq, hgh).
left(pqp, nps).
right(pqp, dxn).
left(lss, nhb).
right(lss, xvp).
left(ckl, mcb).
right(ckl, mvx).
left(lqx, tpf).
right(lqx, ffb).
left(tgd, nsk).
right(tgd, qnm).
left(frp, ttq).
right(frp, cqr).
left(rch, ccb).
right(rch, fdt).
left(vvl, rdj).
right(vvl, skc).
left(nva, kpd).
right(nva, grj).
left(fmm, ftr).
right(fmm, jtk).
left(kpm, mqx).
right(kpm, pcc).
left(qxq, qdt).
right(qxq, jlv).
left(jvj, mrh).
right(jvj, rsm).
left(smn, hpx).
right(smn, cdm).
left(xtp, lld).
right(xtp, qbc).
left(bvb, jcr).
right(bvb, rsq).
left(jtn, tnd).
right(jtn, jfq).
left(clk, qxb).
right(clk, bgq).
left(nmx, ftm).
right(nmx, blm).
left(qkh, bhn).
right(qkh, ktm).
left(vjp, shp).
right(vjp, ltm).
left(fns, snx).
right(fns, gpp).
left(dgh, mlc).
right(dgh, hqq).
left(nfm, cvd).
right(nfm, ctd).
left(nkm, dml).
right(nkm, vct).
left(hmk, fdx).
right(hmk, smk).
left(vvq, fdd).
right(vvq, lhb).
left(fdx, qtg).
right(fdx, jsj).
left(mkl, tdx).
right(mkl, dlc).
left(hqx, ghs).
right(hqx, ltx).
left(kjx, mqb).
right(kjx, ctv).
left(phd, nhk).
right(phd, pgq).
left(kdf, tdd).
right(kdf, pjm).
left(mtk, lcn).
right(mtk, zzz).
left(nvm, lcc).
right(nvm, mkl).
left(xsh, mjv).
right(xsh, frd).
left(llq, grs).
right(llq, cfg).
left(tgp, xck).
right(tgp, mnr).
left(xvj, vlh).
right(xvj, ttx).
left(plr, vvl).
right(plr, sxd).
left(ctr, dhv).
right(ctr, vfq).
left(kkq, vfp).
right(kkq, jng).
left(bmq, gdd).
right(bmq, klx).
left(vvf, lcv).
right(vvf, mgx).
left(pgq, fln).
right(pgq, pdq).
left(ssc, mrn).
right(ssc, jtg).
left(tdd, htb).
right(tdd, tbp).
left(qkj, pch).
right(qkj, lxl).
left(gxv, bxn).
right(gxv, xbh).
left(mrg, rgl).
right(mrg, xmd).
left(kxj, nxp).
right(kxj, jhn).
left(nfd, njf).
right(nfd, bqp).
left(gss, hpp).
right(gss, hqx).
left(xdc, jfc).
right(xdc, jfc).
left(pnf, mgl).
right(pnf, nnm).
left(sqn, lrf).
right(sqn, nhv).
left(hjp, fdx).
right(hjp, smk).
left(rbm, hhb).
right(rbm, ltn).
left(crm, rbd).
right(crm, lbm).
left(dbr, cst).
right(dbr, snc).
left(nqr, ctr).
right(nqr, tfg).
left(gqs, gvr).
right(gqs, cfj).
left(jsk, fvf).
right(jsk, vlm).
left(khr, dhm).
right(khr, gxn).
left(qst, vcj).
right(qst, lnt).
left(dqh, ncv).
right(dqh, lqr).
left(pcq, jrp).
right(pcq, rbm).
left(mmh, cfg).
right(mmh, grs).
left(fhp, css).
right(fhp, nnh).
left(bgb, jvx).
right(bgb, mvn).
left(rck, xps).
right(rck, xnq).
left(xgr, nch).
right(xgr, glv).
left(gcd, cmx).
right(gcd, gjh).
left(hjf, nbf).
right(hjf, tqf).
left(drc, lfp).
right(drc, ffr).
left(rgl, bjr).
right(rgl, dkp).
left(rsm, ldx).
right(rsm, hqb).
left(btc, bqt).
right(btc, mnc).
left(srv, sbl).
right(srv, skv).
left(nfg, jvt).
right(nfg, ssc).
left(cgx, bvk).
right(cgx, xlg).
left(qfh, kdr).
right(qfh, lgt).
left(vct, jjf).
right(vct, dgk).
left(lbm, pkl).
right(lbm, sgh).
left(mks, qgf).
right(mks, jrv).
left(qsg, ltk).
right(qsg, mpv).
left(ggt, bgg).
right(ggt, xhp).
left(gvr, vrd).
right(gvr, cqj).
left(gpd, hqx).
right(gpd, hpp).
left(lld, kdk).
right(lld, xvj).
left(dcg, lrn).
right(dcg, xss).
left(dsj, dfg).
right(dsj, fmm).
left(xmd, dkp).
right(xmd, bjr).
left(mnk, lrn).
right(mnk, xss).
left(ftt, svn).
right(ftt, scg).
left(cmx, srn).
right(cmx, xsc).
left(xvp, bvp).
right(xvp, jbl).
left(bqp, dcg).
right(bqp, mnk).
left(rtm, khf).
right(rtm, qpj).
left(ffr, kpv).
right(ffr, xtp).
left(vln, pjq).
right(vln, ktf).
left(jqx, tnj).
right(jqx, djg).
left(ktf, pdg).
right(ktf, kdf).
left(jvh, lnc).
right(jvh, pxl).
left(bph, njc).
right(bph, dqg).
left(mfj, xhd).
right(mfj, vff).
left(cdc, dlk).
right(cdc, kmf).
left(mxn, lsv).
right(mxn, fcm).
left(lgt, mpb).
right(lgt, png).
left(xft, rct).
right(xft, ppf).
left(bsj, dbk).
right(bsj, ltt).
left(bvt, jsp).
right(bvt, qvg).
left(qbc, xvj).
right(qbc, kdk).
left(vrd, lfg).
right(vrd, fhf).
left(kqt, gvp).
right(kqt, vgj).
left(css, nkm).
right(css, kgq).
left(hxl, dth).
right(hxl, ncl).
left(fxj, drb).
right(fxj, jpr).
left(fft, jvt).
right(fft, ssc).
left(njh, nqk).
right(njh, bjm).
left(ghs, cll).
right(ghs, njh).
left(mmq, jqx).
right(mmq, csx).
left(pgd, fkf).
right(pgd, rgs).
left(pjq, kdf).
right(pjq, pdg).
left(lfg, ksb).
right(lfg, cmm).
left(lrn, bvf).
right(lrn, ksq).
left(ggx, jss).
right(ggx, mts).
left(lcc, tdx).
right(lcc, dlc).
left(hpp, ltx).
right(hpp, ghs).
left(cll, bjm).
right(cll, nqk).
left(tpq, xtc).
right(tpq, tgp).
left(bqq, chv).
right(bqq, vvm).
left(pvj, lnt).
right(pvj, vcj).
left(qnq, qtv).
right(qnq, cmh).
left(cvd, xgf).
right(cvd, nbh).
left(jvt, mrn).
right(jvt, jtg).
left(vfq, bxs).
right(vfq, fcg).
left(cpm, ntv).
right(cpm, rsg).
left(qnf, dlk).
right(qnf, kmf).
left(gca, mgl).
right(gca, nnm).
left(mgx, tfx).
right(mgx, xnn).
left(mhr, njx).
right(mhr, dnt).
left(ktm, gdk).
right(ktm, mqs).
left(kdr, mpb).
right(kdr, png).
left(ndf, xxp).
right(ndf, cjb).
left(zzz, skq).
right(zzz, jkj).
left(mql, fbk).
right(mql, grt).
left(nhv, ckq).
right(nhv, xfd).
left(nbh, pmr).
right(nbh, lpl).
left(kcl, ffb).
right(kcl, tpf).
left(bjr, ftb).
right(bjr, kxr).
left(xsm, qcl).
right(xsm, mtk).
left(kbg, qsg).
right(kbg, lmk).
left(skc, nbq).
right(skc, dkc).
left(png, cnm).
right(png, lqk).
left(htb, rmh).
right(htb, snf).
left(rdm, njx).
right(rdm, dnt).
left(frm, bxn).
right(frm, xbh).
left(xtc, mnr).
right(xtc, xck).
left(jvx, nln).
right(jvx, ccc).
left(gvq, cdm).
right(gvq, hpx).
left(scg, jjd).
right(scg, dbr).
left(ccc, cmr).
right(ccc, nkl).
left(snc, kvk).
right(snc, qtc).
left(gjf, bhb).
right(gjf, ggt).
left(bjk, jps).
right(bjk, bkq).
left(pcc, lrq).
right(pcc, mks).
left(rbv, pvj).
right(rbv, qst).
left(kmf, bqq).
right(kmf, bxg).
left(ksq, hnt).
right(ksq, cpm).
left(lpn, cjb).
right(lpn, xxp).
left(dlk, bxg).
right(dlk, bqq).
left(mpb, cnm).
right(mpb, lqk).
left(vsh, mvj).
right(vsh, gxs).
left(qbq, nbf).
right(qbq, tqf).
left(xlg, lnf).
right(xlg, kcf).
left(lrq, qgf).
right(lrq, jrv).
left(pvq, sgf).
right(pvq, bbc).
left(dss, mjt).
right(dss, qml).
left(tlq, frp).
right(tlq, ldv).
left(dhc, xfc).
right(dhc, dss).
left(frd, pnf).
right(frd, sfz).
left(hgh, stv).
right(hgh, vdl).
left(fhl, fnc).
right(fhl, tjj).
left(dhq, xgr).
right(dhq, nmn).
left(brj, bhr).
right(brj, gbt).
left(cqj, fhf).
right(cqj, lfg).
left(kdk, ttx).
right(kdk, vlh).
left(qjp, nqr).
right(qjp, rjl).
left(gcm, rxh).
right(gcm, khr).
left(tfm, ggr).
right(tfm, bvt).
left(nkl, jhh).
right(nkl, ckl).
left(pfq, lmk).
right(pfq, qsg).
left(xxp, rbv).
right(xxp, qrp).
left(pjm, tbp).
right(pjm, htb).
left(ldg, gvq).
right(ldg, smn).
left(gns, vvf).
right(gns, mfx).
left(tth, mrx).
right(tth, knt).
left(jxl, qjd).
right(jxl, hkk).
left(qkg, xhk).
right(qkg, rnk).
left(rxd, srv).
right(rxd, cxh).
left(sbs, ctl).
right(sbs, bgb).
left(csc, jfc).
right(csc, xsh).
left(skq, rnq).
right(skq, qvn).
left(qss, qcs).
right(qss, qfp).
left(jjf, qqx).
right(jjf, qqx).
left(kmb, lfp).
right(kmb, ffr).
left(lmd, rxd).
right(lmd, ndn).
left(ssq, hmp).
right(ssq, bgk).
left(gkl, pgq).
right(gkl, nhk).
left(dmh, kkj).
right(dmh, mtx).
left(rbd, sgh).
right(rbd, pkl).
left(jbg, rlb).
right(jbg, rrl).
left(xpr, phh).
right(xpr, mxl).
left(pch, tjf).
right(pch, rch).
left(srn, bjk).
right(srn, llt).
left(rgv, nfq).
right(rgv, hmc).
left(mpv, nfm).
right(mpv, cjk).
left(rlb, jdh).
right(rlb, tmt).
left(xqk, csj).
right(xqk, frt).
left(ttx, vjp).
right(ttx, ncg).
left(gpb, knt).
right(gpb, mrx).
left(xkh, gpb).
right(xkh, tth).
left(xjp, fbk).
right(xjp, grt).
left(stf, xkh).
right(stf, qhq).
left(lcn, jkj).
right(lcn, skq).
left(sbl, mnv).
right(sbl, qfh).
left(bqt, qbb).
right(bqt, nmx).
left(dbs, cmc).
right(dbs, kfx).
left(bvr, bsj).
right(bvr, nbp).
left(stv, xfx).
right(stv, qgq).
left(cdm, qfq).
right(cdm, mbf).
left(nrk, rrl).
right(nrk, rlb).
left(qdt, mqn).
right(qdt, cgx).
left(jbl, bmr).
right(jbl, jvj).
left(dbk, hhv).
right(dbk, ktd).
left(jnm, jvl).
right(jnm, kpm).
left(mpk, qxq).
right(mpk, rhk).
left(nrt, nsh).
right(nrt, grc).
left(cmh, hfn).
right(cmh, fxj).
left(kfv, hht).
right(kfv, jsk).
left(xcz, gsr).
right(xcz, cnk).
left(gsr, qqk).
right(gsr, dhq).
left(ppf, qkj).
right(ppf, drk).
left(bgq, krm).
right(bgq, svx).
left(qgf, qbq).
right(qgf, hjf).
left(sfz, nnm).
right(sfz, mgl).
left(xhd, nfd).
right(xhd, pss).
left(ftb, tfm).
right(ftb, cxq).
left(xss, ksq).
right(xss, bvf).
left(sgh, xvg).
right(sgh, pvq).
left(nkc, grc).
right(nkc, nsh).
left(rbp, pqp).
right(rbp, cmp).
left(brr, jgb).
right(brr, gvh).
left(pdt, rlp).
right(pdt, dxm).
left(glv, jtn).
right(glv, fvn).
left(xkj, kpm).
right(xkj, jvl).
left(mqn, xlg).
right(mqn, bvk).
left(mrn, lvr).
right(mrn, mpk).
left(lnf, hct).
right(lnf, dbs).
left(tlt, mcp).
right(tlt, jbc).
left(qvg, nkc).
right(qvg, nrt).
left(vgj, kjf).
right(vgj, sbs).
left(pdb, gvp).
right(pdb, vgj).
left(frt, fhj).
right(frt, gqs).
left(gxn, xvl).
right(gxn, ppz).
left(qhq, gpb).
right(qhq, tth).
left(dqr, rff).
right(dqr, mfm).
left(ttq, jnc).
right(ttq, bkp).
left(jtk, ccq).
right(jtk, mbj).
left(slc, qdg).
right(slc, bsp).
left(tdt, tsp).
right(tdt, skl).
left(vgq, ckr).
right(vgq, kpf).
left(tjj, pgd).
right(tjj, rnc).
left(cxq, ggr).
right(cxq, bvt).
left(fnc, pgd).
right(fnc, rnc).
left(jfq, dtg).
right(jfq, gjl).
left(npm, ggt).
right(npm, bhb).
left(fqd, mcd).
right(fqd, dsj).
left(tqf, qgs).
right(tqf, vvk).
left(svx, tlc).
right(svx, ddl).
left(tdx, bvv).
right(tdx, ngl).
left(msv, ncl).
right(msv, dth).
left(gqq, bvb).
right(gqq, lgs).
left(lnc, pcq).
right(lnc, rvk).
left(bsp, nsb).
right(bsp, csf).
left(mnr, xtx).
right(mnr, trt).
left(pdq, gjm).
right(pdq, tdl).
left(mbf, cft).
right(mbf, stf).
left(qjd, rqp).
right(qjd, vgd).
left(cnm, bpq).
right(cnm, cgg).
left(xls, gss).
right(xls, gpd).
left(tfx, npm).
right(tfx, gjf).
left(dlc, bvv).
right(dlc, ngl).
left(bvk, lnf).
right(bvk, kcf).
left(vgd, dff).
right(vgd, xpk).
left(rgs, lpn).
right(rgs, ndf).
left(jrp, hhb).
right(jrp, ltn).
left(gdd, vst).
right(gdd, svr).
left(bdn, rpd).
right(bdn, qqf).
left(xtx, vmh).
right(xtx, kkv).
left(nmn, nch).
right(nmn, glv).
left(cmc, vgf).
right(cmc, hlg).
left(nbf, vvk).
right(nbf, qgs).
left(lcv, tfx).
right(lcv, xnn).
left(bbc, ctp).
right(bbc, kkq).
left(thr, drc).
right(thr, kmb).
left(gnt, mtx).
right(gnt, kkj).
left(dvq, rtm).
right(dvq, vdx).
left(nfq, vvx).
right(nfq, vcd).
left(ckq, hmv).
right(ckq, xpr).
left(fcg, vgq).
right(fcg, ldr).
left(krm, tlc).
right(krm, ddl).
left(ltx, njh).
right(ltx, cll).
left(vdq, jks).
right(vdq, gns).
left(xsc, llt).
right(xsc, bjk).
left(hqb, gbp).
right(hqb, rfp).
left(vdx, khf).
right(vdx, qpj).
left(rpc, vfr).
right(rpc, mfj).
left(qtc, fft).
right(qtc, nfg).
left(hlm, bsp).
right(hlm, qdg).
left(ggr, qvg).
right(ggr, jsp).
left(xck, xtx).
right(xck, trt).
left(qfp, kvd).
right(qfp, xpb).
left(lmv, csj).
right(lmv, frt).
left(skl, hxl).
right(skl, msv).
left(tvp, djh).
right(tvp, bkc).
left(qml, qkh).
right(qml, smt).
left(mnv, kdr).
right(mnv, lgt).
left(hrc, sbg).
right(hrc, jvh).
left(bjm, kxg).
right(bjm, dqr).
left(tnd, dtg).
right(tnd, gjl).
left(jjn, fqd).
right(jjn, crh).
left(lsv, nrk).
right(lsv, jbg).
left(gbt, clk).
right(gbt, xvf).
left(bgk, qrn).
right(bgk, bdn).
left(blv, ljc).
right(blv, vln).
left(kxr, cxq).
right(kxr, tfm).
left(cmr, jhh).
right(cmr, ckl).
left(xfq, mql).
right(xfq, xjp).
left(mtx, rpc).
right(mtx, rrk).
left(njc, gqq).
right(njc, cqk).
left(fvb, fnc).
right(fvb, tjj).
left(fbx, kxj).
right(fbx, tgm).
left(gpp, xdc).
right(gpp, csc).
left(nrb, jrq).
right(nrb, tlq).
left(rrk, vfr).
right(rrk, mfj).
left(tks, jjn).
right(tks, dmb).
left(gjh, xsc).
right(gjh, srn).
left(cfj, cqj).
right(cfj, vrd).
left(mfx, lcv).
right(mfx, mgx).
left(xtr, qfp).
right(xtr, qcs).
left(qgq, dkt).
right(qgq, xsm).
left(cfz, grj).
right(cfz, kpd).
left(jgb, hrc).
right(jgb, lrr).
left(rdj, dkc).
right(rdj, nbq).
left(grs, xqg).
right(grs, dpn).
left(jps, xkj).
right(jps, jnm).
left(jss, ljj).
right(jss, tdt).
left(qpj, rdm).
right(qpj, mhr).
left(dcn, smn).
right(dcn, gvq).
left(pkl, xvg).
right(pkl, pvq).
left(sxa, hcb).
right(sxa, qkg).
left(ltn, hjp).
right(ltn, hmk).
left(lvr, qxq).
right(lvr, rhk).
left(hfn, jpr).
right(hfn, drb).
left(jdh, hkc).
right(jdh, ggx).
left(mrx, shb).
right(mrx, jdb).
left(lfp, kpv).
right(lfp, xtp).
left(dkp, kxr).
right(dkp, ftb).
left(xpg, qnf).
right(xpg, cdc).
left(mrh, ldx).
right(mrh, hqb).
left(xfx, dkt).
right(xfx, dkt).
left(kkx, hdb).
right(kkx, dqh).
left(svr, kfp).
right(svr, ftt).
left(vcj, qmn).
right(vcj, bvr).
left(lqh, mkl).
right(lqh, lcc).
left(mjt, qkh).
right(mjt, smt).
left(qrp, qst).
right(qrp, pvj).
left(tmp, mcp).
right(tmp, jbc).
left(hlg, pfs).
right(hlg, vvq).
left(llv, cmh).
right(llv, qtv).
left(qbb, blm).
right(qbb, ftm).
left(ctv, htm).
right(ctv, xft).
left(dnt, kfg).
right(dnt, brr).
left(xhk, brj).
right(xhk, qcb).
left(cjk, ctd).
right(cjk, cvd).
left(aaa, jkj).
right(aaa, skq).
left(bxx, gnt).
right(bxx, dmh).
left(hcb, rnk).
right(hcb, xhk).
left(lnt, qmn).
right(lnt, bvr).
left(tgm, nxp).
right(tgm, jhn).
left(cqk, lgs).
right(cqk, bvb).
left(cjb, rbv).
right(cjb, qrp).
left(hdb, ncv).
right(hdb, lqr).
left(cnq, ctv).
right(cnq, mqb).
left(bhn, gdk).
right(bhn, gdk).
left(dml, jjf).
right(dml, dgk).
left(fvf, krh).
right(fvf, bmq).
left(tsp, msv).
right(tsp, hxl).
left(qbg, vlr).
right(qbg, qjp).
left(knt, jdb).
right(knt, shb).
left(fcs, tgm).
right(fcs, kxj).
left(fvn, jfq).
right(fvn, tnd).
left(dhv, fcg).
right(dhv, bxs).
left(xpk, vhv).
right(xpk, thr).
left(bkp, vdq).
right(bkp, hpt).
left(vst, ftt).
right(vst, kfp).
left(dls, vvl).
right(dls, sxd).
left(ftr, mbj).
right(ftr, ccq).
left(mxl, crm).
right(mxl, vqb).
left(dhp, dhc).
right(dhp, pqq).
left(bxs, ldr).
right(bxs, vgq).
left(dql, hdb).
right(dql, dqh).
left(tmt, ggx).
right(tmt, hkc).
left(pfc, cjf).
right(pfc, cfz).
left(ctd, xgf).
right(ctd, nbh).
left(vfr, vff).
right(vfr, xhd).
left(rqp, dff).
right(rqp, xpk).
left(ltk, nfm).
right(ltk, cjk).
left(mts, ljj).
right(mts, tdt).
left(hht, fvf).
right(hht, vlm).
left(mnc, qbb).
right(mnc, nmx).
left(fkf, lpn).
right(fkf, ndf).
left(lhd, qjd).
right(lhd, hkk).
left(jfc, mjv).
right(jfc, mjv).
left(xxx, lmd).
right(xxx, lgh).
left(ncv, hff).
right(ncv, fhp).
left(pss, bqp).
right(pss, njf).
left(nqh, xls).
right(nqh, fqg).
left(vgf, vvq).
right(vgf, pfs).
left(skv, mnv).
right(skv, qfh).
left(ljc, ktf).
right(ljc, pjq).
left(snx, xdc).
right(snx, csc).
left(ndn, cxh).
right(ndn, srv).
left(hkc, mts).
right(hkc, jss).
left(phh, crm).
right(phh, vqb).
left(fln, gjm).
right(fln, tdl).
left(vvk, phd).
right(vvk, gkl).
left(mqs, rkn).
right(mqs, pfc).
left(lrr, jvh).
right(lrr, sbg).
left(jkj, rnq).
right(jkj, qvn).
left(xvl, hcb).
right(xvl, qkg).
left(npd, snx).
right(npd, gpp).
left(ktd, qdj).
right(ktd, khk).
left(tdl, rgv).
right(tdl, glb).
left(snf, hlj).
right(snf, xxx).
left(mbj, jmk).
right(mbj, bdb).
left(gdk, rkn).
right(gdk, rkn).
left(sfv, dls).
right(sfv, plr).
left(knf, bgk).
right(knf, hmp).
left(xvf, qxb).
right(xvf, bgq).
left(rmh, hlj).
right(rmh, xxx).
left(lpf, xvp).
right(lpf, nhb).
left(nsh, kqj).
right(nsh, mbt).
left(qnm, gxv).
right(qnm, frm).
left(ltm, bkl).
right(ltm, skr).
left(gjl, tcx).
right(gjl, lvm).
left(kxg, rff).
right(kxg, mfm).
left(tfg, dhv).
right(tfg, vfq).
left(hpx, mbf).
right(hpx, qfq).
left(gjm, glb).
right(gjm, rgv).
left(vcd, tvh).
right(vcd, gcm).
left(nln, nkl).
right(nln, cmr).
left(xpb, tvp).
right(xpb, qrt).
left(frj, xtc).
right(frj, tgp).
left(fhf, ksb).
right(fhf, cmm).
left(kvk, fft).
right(kvk, nfg).
left(grc, mbt).
right(grc, kqj).
left(bhr, xvf).
right(bhr, clk).
left(hlj, lmd).
right(hlj, lgh).
left(llt, jps).
right(llt, bkq).
left(dkc, bpl).
right(dkc, hgh).
left(bpl, stv).
right(bpl, vdl).
left(shb, fbx).
right(shb, fcs).
left(fqg, gpd).
right(fqg, gss).
left(tln, rcq).
right(tln, rbp).
left(ldr, ckr).
right(ldr, kpf).
left(nch, jtn).
right(nch, fvn).
left(cmm, bph).
right(cmm, mxs).
left(rlp, lpf).
right(rlp, lss).
left(ckr, shl).
right(ckr, nqh).
left(tjf, fdt).
right(tjf, ccb).
left(bvd, dls).
right(bvd, plr).
left(qqk, xgr).
right(qqk, nmn).
left(kpv, qbc).
right(kpv, lld).
left(hff, nnh).
right(hff, css).
left(dkg, pqq).
right(dkg, dhc).
left(tqz, dmb).
right(tqz, jjn).
left(mlc, vnn).
right(mlc, xpg).
left(dhx, pdb).
right(dhx, kqt).
left(bhb, xhp).
right(bhb, bgg).
left(rhk, qdt).
right(rhk, jlv).
left(pdg, pjm).
right(pdg, tdd).
left(fhj, cfj).
right(fhj, gvr).
left(djg, ksd).
right(djg, dhx).
left(rff, mxn).
right(rff, pmm).
left(mqb, htm).
right(mqb, xft).
left(nsb, btc).
right(nsb, mld).
left(ddl, frj).
right(ddl, tpq).
left(xdb, vdx).
right(xdb, rtm).
left(njx, kfg).
right(njx, brr).
left(jhh, mvx).
right(jhh, mcb).
left(dff, thr).
right(dff, vhv).
left(gma, jjn).
right(gma, dmb).
left(shp, bkl).
right(shp, skr).
left(qxb, krm).
right(qxb, svx).
