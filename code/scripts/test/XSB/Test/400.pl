vP(V,H):-vP0(V, H).
vP(V1,H):-a(V1,V2),vP(V2,H).
vP(V2,H2):-l(V1,F,V2),vP(V1,H1),hP(H1,F,H2).
hP(H1,F,H2):-s(V1,F,V2),vP(V1,H1),vP(V2,H2).

l(_,_,_):-fail.
a(0,3082).
a(4349,0).
a(9571,0).
a(0,4485).
a(0,4836).
a(4541,0).
a(5702,0).
a(0,5799).
a(0,8213).
a(1371,0).
a(0,6947).
a(2951,0).
a(1487,0).
a(0,10241).
a(0,6881).
a(0,1477).
a(5221,0).
a(5361,0).
a(5621,0).
a(0,5010).
a(9133,0).
a(0,4056).
a(1400,0).
a(7752,7743).
a(0,1588).
a(0,2611).
a(4126,0).
a(3788,0).
a(0,5542).
a(0,8515).
a(8834,8769).
a(0,7268).
a(6890,0).
a(6932,0).
a(4312,0).
a(0,5850).
a(0,9148).
a(5168,5159).
a(0,9206).
a(0,1795).
a(0,6041).
a(0,5122).
a(0,4785).
a(2774,0).
a(7734,0).
a(0,4267).
a(0,4196).
a(8841,0).
a(3036,0).
a(0,7899).
a(6336,0).
a(4483,0).
a(4053,0).
a(0,4443).
a(7162,0).
a(0,6742).
a(5375,0).
a(0,6048).
a(7542,0).
a(0,8673).
a(3022,0).
a(0,8411).
a(0,7758).
a(0,2629).
a(9533,0).
a(5885,0).
a(0,6567).
a(8444,0).
a(4465,0).
a(0,4744).
a(0,7052).
a(9082,0).
a(0,10340).
a(901,0).
a(7914,0).
a(0,7070).
a(7712,0).
a(0,2853).
a(4398,0).
a(3994,3986).
a(3481,0).
a(0,2512).
a(0,3957).
a(0,3237).
a(0,9775).
a(3774,0).
a(2998,0).
a(7785,0).
a(0,5898).
a(0,3092).
a(0,9351).
a(0,4111).
a(0,7984).
a(6782,0).
a(7530,0).
a(0,8441).
a(0,1640).
a(0,8891).
a(0,2204).
a(6623,0).
a(1824,0).
a(8832,8767).
a(6153,6230).
a(4861,0).
a(7715,0).
a(0,10252).
a(10082,0).
a(8006,0).
a(5036,0).
a(0,4378).
a(0,8516).
a(8233,0).
a(8489,0).
a(6652,0).
a(0,4122).
a(0,2340).
a(4698,0).
a(0,3343).
a(2179,0).
a(2606,0).
a(0,9075).
a(9234,9233).
a(0,2782).
a(1505,0).
a(2474,2472).
a(8867,0).
a(9060,0).
a(4761,0).
a(0,9137).
a(6681,0).
a(0,5630).
a(0,5254).
a(1489,0).
a(2582,0).
a(4282,0).
a(0,7549).
a(0,3795).
a(8293,0).
a(0,2435).
a(0,8163).
a(0,8187).
a(5845,0).
a(0,4995).
a(8200,0).
a(1665,0).
a(7853,0).
a(0,4749).
a(5823,0).
a(2467,0).
a(9026,0).
a(0,1825).
a(3772,0).
a(5661,0).
a(8830,8802).
a(5524,0).
a(9347,0).
a(0,8235).
a(0,1647).
a(0,3738).
a(2506,0).
a(4904,0).
a(8481,0).
a(0,9554).
a(0,6028).
a(0,8971).
a(8879,0).
a(7697,0).
a(0,5875).
a(0,3674).
a(6654,6646).
a(0,8143).
a(7875,7865).
a(9685,0).
a(7209,0).
a(0,10280).
a(7527,0).
a(108,0).
a(0,1808).
a(0,2849).
a(5727,0).
a(1429,0).
a(5289,0).
a(2183,0).
a(7327,0).
a(0,4004).
a(5908,0).
a(9751,0).
a(8838,0).
a(0,2931).
a(0,5530).
a(0,2460).
a(6339,0).
a(5510,0).
a(0,5099).
a(0,6899).
a(0,8593).
a(0,9743).
a(2949,0).
a(8184,0).
a(0,10096).
a(0,9217).
a(0,8829).
a(0,5521).
a(4991,0).
a(5723,0).
a(0,9840).
a(0,7638).
a(0,7724).
a(9036,0).
a(9344,9340).
a(5488,0).
a(9885,0).
a(0,9197).
a(7694,0).
a(10236,0).
a(10261,0).
a(6627,0).
a(0,4513).
a(7427,0).
a(0,6037).
a(0,3768).
a(0,3855).
a(5205,0).
a(6879,0).
a(0,9703).
a(7115,0).
a(0,5005).
a(0,10029).
a(0,5718).
a(0,6419).
a(5735,0).
a(0,4478).
a(0,7701).
a(4956,0).
a(3809,0).
a(7068,0).
a(2846,0).
a(2596,0).
a(2713,0).
a(7150,0).
a(8051,0).
a(1611,0).
a(3961,0).
a(0,3438).
a(8832,8745).
a(8156,0).
a(0,9109).
a(4552,4546).
a(0,4539).
a(4326,0).
a(0,3586).
a(0,3614).
a(0,9893).
a(0,5994).
a(6094,0).
a(4978,0).
a(6115,0).
a(0,2738).
a(3434,0).
a(0,4347).
a(7375,0).
a(0,6533).
a(5378,0).
a(10090,0).
a(3089,0).
a(6511,0).
a(5677,0).
a(3133,0).
a(0,1827).
a(9077,0).
a(5882,0).
a(0,2549).
a(4624,0).
a(3999,0).
a(1453,0).
a(0,6025).
a(0,2790).
a(3849,0).
a(5598,0).
a(0,7699).
a(9722,0).
a(0,8409).
a(8834,8773).
a(0,4658).
a(8081,0).
a(0,9022).
a(6555,6548).
a(0,9055).
a(1390,0).
a(6957,0).
a(5443,0).
a(6608,0).
a(0,6684).
a(0,4984).
a(2835,0).
a(8917,0).
a(3151,0).
a(6431,6427).
a(1676,0).
a(7566,7565).
a(0,7082).
a(0,5257).
a(3984,0).
a(6545,0).
a(7029,0).
a(10009,0).
a(0,5653).
a(0,5827).
a(8831,8791).
a(6212,6210).
a(0,8124).
a(0,8567).
a(6566,0).
a(0,7159).
a(0,3815).
a(0,5296).
a(6589,0).
a(0,6033).
a(2777,0).
a(0,9090).
a(0,5684).
a(7726,0).
a(0,6352).
a(8885,0).
a(0,4859).
a(0,5190).
a(0,125).
a(0,6918).
a(5216,0).
a(0,6381).
a(5340,0).
a(0,6228).
a(0,4289).
a(8240,0).
a(0,2544).
a(6909,0).
a(2473,0).
a(6747,0).
a(9301,0).
a(3749,0).
a(0,7669).
a(10104,0).
a(0,4678).
a(2578,0).
a(3317,0).
a(0,2496).
a(0,6326).
a(0,2191).
a(4617,0).
a(9583,0).
a(1486,1483).
a(0,3699).
a(0,9444).
a(0,1649).
a(0,3229).
a(0,1393).
a(0,9043).
a(3791,0).
a(5712,0).
a(7815,0).
a(5093,0).
a(0,4114).
a(6130,0).
a(5000,0).
a(0,3923).
a(2640,0).
a(8618,0).
a(0,4338).
a(9294,0).
a(8931,8928).
a(5687,0).
a(8923,0).
a(8053,0).
a(5019,0).
a(2426,0).
a(6316,0).
a(0,10328).
a(6347,0).
a(8668,0).
a(0,7487).
a(3896,0).
a(8439,0).
a(8873,0).
a(8831,8743).
a(0,8285).
a(0,8612).
a(0,7118).
a(0,5930).
a(3080,0).
a(0,9164).
a(9754,0).
a(4654,0).
a(0,2470).
a(0,3029).
a(3535,0).
a(3844,0).
a(4059,0).
a(0,8148).
a(0,8275).
a(5057,0).
vP0(0,0).
vP0(1,48).
vP0(2,57).
vP0(3,66).
vP0(4,1).
vP0(5,2).
vP0(6,3).
vP0(7,4).
vP0(8,5).
vP0(9,6).
vP0(10,7).
vP0(11,8).
vP0(12,25).
vP0(13,9).
vP0(14,21).
vP0(15,10).
vP0(16,11).
vP0(17,12).
vP0(18,13).
vP0(19,14).
vP0(20,15).
vP0(21,16).
vP0(22,17).
vP0(23,85).
vP0(24,18).
vP0(25,76).
vP0(26,19).
vP0(27,20).
vP0(28,77).
vP0(29,22).
vP0(30,23).
vP0(31,24).
vP0(32,31).
vP0(33,26).
vP0(34,27).
vP0(35,65).
vP0(36,56).
vP0(37,28).
vP0(38,29).
vP0(39,30).
vP0(40,41).
vP0(41,63).
vP0(42,71).
vP0(43,32).
vP0(44,74).
vP0(45,35).
vP0(46,33).
vP0(47,34).
vP0(48,37).
vP0(49,58).
vP0(50,89).
vP0(51,36).
vP0(52,39).
vP0(53,38).
vP0(54,40).
vP0(55,42).
vP0(56,43).
vP0(57,44).
vP0(58,45).
vP0(59,46).
vP0(60,55).
vP0(61,75).
vP0(62,47).
vP0(63,49).
vP0(64,50).
vP0(65,51).
vP0(66,52).
vP0(67,53).
vP0(68,54).
vP0(69,72).
vP0(70,79).
vP0(71,62).
vP0(72,73).
vP0(73,78).
vP0(74,87).
vP0(75,59).
vP0(76,60).
vP0(77,61).
vP0(78,64).
vP0(79,69).
vP0(80,84).
vP0(81,67).
vP0(82,68).
vP0(83,70).
vP0(84,81).
vP0(85,80).
vP0(86,83).
vP0(87,86).
vP0(88,82).
vP0(89,88).
vP0(90,90).
vP0(1389,91).
vP0(1389,92).
vP0(1389,96).
vP0(1389,97).
vP0(1389,98).
vP0(1389,99).
vP0(1389,100).
vP0(1389,102).
vP0(1389,103).
vP0(1389,105).
vP0(1389,106).
vP0(1389,108).
vP0(1389,109).
vP0(1389,111).
vP0(1389,112).
vP0(1389,114).
vP0(1389,115).
vP0(1389,117).
vP0(1389,118).
vP0(1389,120).
vP0(1389,121).
vP0(1389,123).
vP0(1389,124).
vP0(1389,125).
vP0(1389,126).
vP0(1389,127).
vP0(1389,128).
vP0(1389,129).
vP0(1389,130).
vP0(1389,131).
vP0(1389,132).
vP0(1389,133).
vP0(1389,134).
vP0(1389,135).
vP0(1389,136).
vP0(1389,137).
vP0(1389,138).
vP0(1389,139).
vP0(1389,140).
vP0(1389,141).
vP0(1389,142).
vP0(1389,143).
vP0(1389,144).
vP0(1389,145).
vP0(1389,146).
vP0(1389,147).
vP0(1389,148).
vP0(1389,149).
vP0(1389,150).
vP0(1389,151).
vP0(1389,152).
vP0(1389,153).
vP0(1389,154).
vP0(1389,155).
vP0(1389,156).
vP0(1389,157).
vP0(1389,158).
vP0(1389,160).
vP0(1389,161).
vP0(1389,163).
vP0(1389,164).
vP0(1389,166).
vP0(1389,167).
vP0(1389,169).
vP0(1389,170).
vP0(1389,172).
vP0(1389,173).
vP0(1389,175).
vP0(1389,176).
vP0(1389,178).
vP0(1389,179).
vP0(1389,181).
vP0(1389,182).
vP0(1389,184).
vP0(1389,185).
vP0(1389,186).
vP0(1389,187).
vP0(1389,189).
vP0(1389,190).
vP0(1389,192).
vP0(1389,193).
vP0(1389,195).
vP0(1389,196).
vP0(1389,198).
vP0(1389,199).
vP0(1389,201).
vP0(1389,202).
vP0(1389,204).
vP0(1389,205).
vP0(1389,207).
vP0(1389,208).
vP0(1389,210).
vP0(1389,211).
vP0(1389,213).
vP0(1389,214).
vP0(1389,215).
vP0(1389,216).
vP0(1389,218).
vP0(1389,219).
vP0(1389,220).
vP0(1389,221).
vP0(1389,222).
vP0(1389,223).
vP0(1389,224).
vP0(1389,225).
vP0(1389,227).
vP0(1389,228).
vP0(1389,229).
vP0(1389,230).
vP0(1389,232).
vP0(1389,233).
vP0(1389,235).
vP0(1389,236).
vP0(1389,238).
vP0(1389,239).
vP0(1389,241).
vP0(1389,242).
vP0(1389,244).
vP0(1389,245).
vP0(1389,247).
vP0(1389,248).
vP0(1389,250).
vP0(1389,251).
vP0(1389,253).
vP0(1389,254).
vP0(1389,256).
vP0(1389,257).
vP0(1389,259).
vP0(1389,260).
vP0(1389,262).
vP0(1389,263).
vP0(1389,265).
vP0(1389,266).
vP0(1389,268).
vP0(1389,269).
vP0(1389,270).
vP0(1389,271).
vP0(1389,272).
vP0(1389,273).
vP0(1389,274).
vP0(1389,275).
vP0(1389,276).
vP0(1389,277).
vP0(1389,278).
vP0(1389,279).
vP0(1389,280).
vP0(1389,281).
vP0(1389,282).
vP0(1389,283).
vP0(1389,284).
vP0(1389,285).
vP0(1389,287).
vP0(1389,288).
vP0(1389,289).
vP0(1389,290).
vP0(1389,291).
vP0(1389,292).
vP0(1389,293).
vP0(1389,294).
vP0(1389,295).
vP0(1389,296).
vP0(1389,298).
vP0(1389,299).
vP0(1389,301).
vP0(1389,302).
vP0(1389,304).
vP0(1389,305).
vP0(1389,307).
vP0(1389,308).
vP0(1389,310).
vP0(1389,311).
vP0(1389,313).
vP0(1389,314).
vP0(1389,316).
vP0(1389,317).
vP0(1389,319).
vP0(1389,320).
vP0(1389,322).
vP0(1389,323).
vP0(1389,324).
vP0(1389,325).
vP0(1389,326).
vP0(1389,327).
vP0(1389,328).
vP0(1389,329).
vP0(1389,330).
vP0(1389,331).
vP0(1389,332).
vP0(1389,333).
vP0(1389,335).
vP0(1389,336).
vP0(1389,338).
vP0(1389,339).
vP0(1389,341).
vP0(1389,342).
vP0(1389,344).
vP0(1389,345).
vP0(1389,347).
vP0(1389,348).
vP0(1389,350).
vP0(1389,351).
vP0(1389,353).
vP0(1389,354).
vP0(1389,356).
vP0(1389,357).
vP0(1389,359).
vP0(1389,360).
vP0(1389,362).
vP0(1389,363).
vP0(1389,365).
vP0(1389,366).
vP0(1389,368).
vP0(1389,369).
vP0(1389,371).
vP0(1389,372).
vP0(1389,374).
vP0(1389,375).
vP0(1389,377).
vP0(1389,378).
vP0(1389,380).
vP0(1389,381).
vP0(1389,382).
vP0(1389,383).
vP0(1389,384).
vP0(1389,385).
vP0(1389,386).
vP0(1389,387).
vP0(1389,388).
vP0(1389,389).
vP0(1389,390).
vP0(1389,391).
vP0(1389,393).
vP0(1389,394).
vP0(1389,396).
vP0(1389,397).
vP0(1389,399).
vP0(1389,400).
vP0(1389,402).
vP0(1389,403).
vP0(1389,405).
vP0(1389,406).
vP0(1389,408).
vP0(1389,409).
vP0(1389,411).
vP0(1389,412).
vP0(1389,413).
vP0(1389,414).
vP0(1389,416).
vP0(1389,417).
vP0(1389,419).
vP0(1389,420).
vP0(1389,422).
vP0(1389,423).
vP0(1389,425).
vP0(1389,426).
vP0(1389,428).
vP0(1389,429).
vP0(1389,431).
vP0(1389,434).
vP0(1389,435).
vP0(1389,436).
vP0(1389,437).
vP0(1389,438).
vP0(1389,439).
vP0(1389,440).
vP0(1389,441).
vP0(1389,442).
vP0(1389,443).
vP0(1389,444).
vP0(1389,445).
vP0(1389,446).
vP0(1389,447).
vP0(1389,448).
vP0(1389,449).
vP0(1389,450).
vP0(1389,451).
vP0(1389,452).
vP0(1389,453).
vP0(1389,454).
vP0(1389,455).
vP0(1389,456).
vP0(1389,457).
vP0(1389,458).
vP0(1389,459).
vP0(1389,460).
vP0(1389,461).
vP0(1389,462).
vP0(1389,463).
vP0(1389,464).
vP0(1389,465).
vP0(1389,466).
vP0(1389,467).
vP0(1389,468).
vP0(1389,469).
vP0(1389,470).
vP0(1389,471).
vP0(1389,472).
vP0(1389,473).
vP0(1389,474).
vP0(1389,475).
vP0(1389,476).
vP0(1389,477).
vP0(1389,478).
vP0(1389,479).
vP0(1389,480).
vP0(1389,481).
vP0(1389,482).
vP0(1389,483).
vP0(1389,484).


  :- table vP/2.

  :- import length/2 from basics.

printResults(L) :- open(file('xsb.output'),write,Stream), printList(L,Stream).

printList([],Stream) :- close(Stream).
printList([(X,Y)|L],Stream) :- fmt_write(Stream,"%d\t%d\n",args(X,Y)), printList(L,Stream).

run(Length) :- findall((X,Y),vP(X,Y),L), length(L,Length),printResults(L).
