%% mydata_my_pet
% Sets referenced data

%%
function [data, txt_data, metadata] = mydata_Pseudemydura_umbrina 
  % created by Starrlight Augustine, Bas Kooijman, Dina Lika, Goncalo Marques and Laure Pecquerie 2015/03/31
  
  %% Syntax
  % [data, txt_data, metadata] = <../mydata_my_pet.m *mydata_my_pet*>
  
  %% Description
  % Sets data, pseudodata, metadata, explanatory text, weight coefficients.
  % Meant to be a template in add_my_pet
  %
  % Output
  %
  % * data: structure with data
  % * txt_data: text vector for the presentation of results
  % * metadata: structure with info about this entry
  
  %% To do (remove these remarks after editing this file)
  % * copy this template; replace 'my_pet' by the name of your species
  % * fill in metadata fields with the proper information
  % * insert references for the data (an example is given)
  % * edit fact-list for your species, where you can add species and/or data properties
  % * edit real data; remove all data that to not belong to your pet
  % * complete reference list
  % * OPTIONAL : add discussion points / comments before the reference list

%% set metadata

T_C = 273.15; % K, temperature at 0 degrees C (used in T_typical)

metadata.phylum     = 'Chordata'; 
metadata.class      = 'Reptilia'; 
metadata.order      = 'Testudines'; 
metadata.family     = 'Chelidae';
metadata.species    = 'Pseudemydura_umbrina'; 
metadata.species_en = 'Western Swamp Turtle';
metadata.T_typical  = T_C + 17.7; % K
metadata.data_0     = {'ab'; 'ap'; 'am'; 'Lb'; 'Lp'; 'Li'; 'Wdb'; 'Wdp'; 'Wdi'; 'Ri'};  % tags for different types of zero-variate data
%metadata.data_1     = {'t-L', 'L-W', 'T_O'}; % tags for different types of uni-variate data
metadata.data_1     = {'T_O'}; % tags for different types of uni-variate data

metadata.COMPLETE = 3.3; % Using criteria of LikaKear2011

metadata.author   = {'Sophie Arnall'};                       
metadata.date_acc = [2013 05 03];                         
metadata.email    = {'arnals01@student.uwa.edu.au'};
metadata.address  = {'University of Western Australia, 6009, Australia'}; 

% uncomment and fill in the following fields when the entry is updated:
% metadata.author_mod_1  = {'author2'};                       % put names as authors as separate strings:  {'author1','author2'} , with corresponding author in first place 
% metadata.date_mod_1    = [2017 09 18];                      % [year month day], date modified entry is accepted into the collection
% metadata.email_mod_1   = {'myname@myuniv.univ'};            % e-mail of corresponding author
% metadata.address_mod_1 = {'affiliation, zipcode, country'}; % affiliation, postcode, country of the corresponding author

%% set data
% zero-variate data;
% typically depend on scaled functional response f.
% here assumed to be equal for all real data; the value of f is specified in pars_init_my_pet.

% age 0 is at onset of embryo development
data.ab = 250;      units.ab = 'd';    label.ab = 'age at birth';                bibkey.ab = 'Mitchell_et_al_2006';
  temp.ab = T_C + 24.532;  bibkey.ab = 'Mitchell_et_al_2006'; % K, temperature, based on ;
  % observed age at birth is frequently larger than ab, because of diapauzes during incubation
data.ap = data.ab+11*365;     units.ap = 'd';    label.ap = 'age at puberty';              bibkey.ap = 'Jarvie_unpub';
  temp.ap = T_C + 21;  bibkey.ap = 'Jarvie_unpub'; % K, temperature, based on simulation of Tb from 2000-2013 at Orford, see last lines of Sphenodon_punctatus.R;;
  % observed age at puberty is frequently larger than ap, 
  %   because allocation to reproduction starts before first eggs appear
data.am = 100*365;     units.am = 'd';    label.am = 'life span';                   bibkey.am = 'Dawbin_1982';
  temp.am = T_C + 21;  bibkey.am = 'Jarvie_unpub'; % K, temperature, based on simulation of Tb from 2000-2013 at Stephens Island/Takapourewa, see last lines of Sphenodon_punctatus.R;;
% (accounting for aging only) 

% Please specify what type of length measurement is used for your species.
% We put here snout-to-vent length, but you should change this depending on your species:
data.Lb  = 2.6;   units.Lb  = 'cm';   label.Lb  = 'snout to vent length at birth';    bibkey.Lb  = 'Cree_unpub';
data.Lp  = 10.7;   units.Lp  = 'cm';   label.Lp  = 'snout to vent length at puberty';  bibkey.Lp  = 'Cree_1994';
data.Li  = 13.1;   units.Li  = 'cm';   label.Li  = 'ultimate snout to vent length';    bibkey.Li  = 'Jarvie_unpub';
data.Wdb = 5.2*0.3; units.Wdb = 'g';    label.Wdb = 'dry weight at birth';              bibkey.Wdb = 'Cree_unpub';
%find dry weight at puberty
data.Wdp = 236.5*0.3;   units.Wdp = 'g';    label.Wdp = 'dry weight at puberty';            bibkey.Wdp = 'Cree_1994';
data.Wdi = 417*0.3;   units.Wdi = 'g';    label.Wdi = 'ultimate dry weight';              bibkey.Wdi = 'Dawbin_1982';
data.Ri  = 5/365;    units.Ri  = '#/d';  label.Ri  = 'maximum reprod rate';              bibkey.Ri  = 'Cree_1994';
  % for an individual of ultimate length Li 
  temp.Ri = T_C +  15.52;  bibkey.Ri = 'Jarvie_unpub'; % K, temperature, based on simulation of Tb from 2000-2013 at Stephens Island/Takapourewa, see last lines of Sphenodon_punctatus traits.R;
 
% uni-variate data

% uni-variate data at f = 1.0 (this value should be added in pars_init_my_pet as a parameter f_tL) 
% snout-to-vent length and wet weight were measured at the same time
%data.tL = [0	539.105	1036.6	1526.43	2023.925	2573.615	3170.39	3872.285	4534.395	5254.905	5982.715	6844.115	7784.355	8729.705	9691.115	10644.13	11670.875	12692.145	13765.975	14779.58	15753.765	16809.345	17830.98	18897.145	19950.17	20934.94	21851.09;    % d, time since birth
%           5.037	6.256	7.4811	8.725	9.8895	11.0948	12.2177	13.3859	14.4665	15.4192	16.2948	17.0951	17.8422	18.5323	19.1243	19.6361	20.0568	20.3866	20.6559	20.9325	21.1449	21.3802	21.5316	21.6226	21.7174	21.7815	21.8688]';  % cm, snout-to-vent length at f and T
%units.tL = {'d', 'cm'};     label.tL = {'time since birth', 'snout to vent length'};  bibkey.tL = 'Dawbin_1982';
%  temp.tL = T_C + 15.52;  % K, temperature

%data.LW = [12.6	13.2	21.2	16.9	18	20.2	21.2	22.4	24.4	23.5	21.8	21.4	21	23.4	22.2	18.8	21	20	21	20.2	19.3	20.3	20.1	22	20.2	20	18.6	17.8	10.1	16.8	23.7	20.1	19.2	17.5	21.5	21.8	23.9	19	19.5	21.5	21.3	19.6	13	19.8	19.1	19.9	21	21.7	20.4	22.5	18.4	21.5	22.5	21.2	29.1	18	19.2	23.4	21.1	21.5	21.4	21.5	21.9	21.2	19.7	19.7	20.8	22.9	20.7	20.5	20	19.1	21.6	17.4	17.5	20.2	19.6	19	22.2	18.3	21.5	17.4	18.7	21.3	19.7	19.7	18.6	20.6	19	20.9	18.9	19.8	18	20.6	21.9	20.9	20.2	21.3	20	18.8	19.6	20.7	18	20.3	19.6	21.3	17.4	20	21.5	18.2	19.7	13.9	21.1	22.3	18.1	19.9	22.8	21.8	19.3	19.6	19.6	22.9	19.9	18	19.4	21.8	21.2	20.8	22.5	19.2	18.7	20.7	16.3	21.8	14.5	20.5	18.6	19.6	17.6	19.4	21.7	18.4	20.4	20.8	19.5	19.6	18.2	20.8	19.6	21.5	18.3	18.4	21.5	19.5	21.3	21.1	20.6	21.5	20.3	20.5	21.7	20.1	18.6	20.1	18.7	18.6	18.8	21.3	19.6	20.8	18.8	21.1	18.9	18.2	21.9	18.2	20.8	19.8	20.3	19.6	21.5	20.9	20.9	18	14	17.9	18.7	18.6	18.8	19.7	16.5	20.1	19.2	19.7	18.7	21.5	19.9	21	16.3	21.2	20.7	19.1	21.1	18.9	14.6	21.6	18.8	19.8	18	21.8	20.8	17.4	22.1	19.1	20.8	17.2	20.4	18.7	16.2	18.7	19.1	20.3	20.1	19.3	11.1	22.3	19.9	19.8	18.8	20	17.3	20	20.7	14.6	21.2	21.7	21.8	23	16.1	23.2	19.2	17.2	17.5	19.4	20.2	21.2	21	17.8	21	15	19.4	19.6	20.6	19.7	14.3	21.6	19.1	15.9	23	19.3	18.4	21.1	20.2	16.1	21	20.7	18.4	20	24.5	19	20.3	18.7	18.9	17.9	19.4	19.3	22.1	19.4	21	12.6	21.1	18.2	19.7	20.5	21.7	19.8	21.5	22.3	18.4	18.8	22	18.5	20.7	20	19.3	20.3	19.3	19.8	19.7	13.4	15.9	13.3	23.1	17.9	18.2	20.8	11.9	20.1	21.5	13.6	20.7	20.8	20.4	21.5	15.5	21.4	15.7	21.1	18.9	20.6	20.8	20.1	17.9	20.4	20.2	20.2	20.8	19.2	21.9	18.2	21	17.8	22.4	20.4	19.7	20	18.6	21.5	13.7	20.2	21.7	23.2	19.1	20	23.2	20.9	18.9	21.7	19.2	20.5	20.5	20.8	19.6	20.8	19.6	21.7	17	21.4	18.1	22.7	22.7	20.8	20.5	21.4	19.7	18.1	17.7	17	18.3	20.2	19.4	19	20.7	21.2	20.2	20.1	20	19.3	19.4	18.8	18.5	19.4	22.1	19.1	20.3	19.2	18.5	18.7	19.2	23.5	20.5	20.1	19.1	18	18.4	19.8	21.4	21.6	19	20.3	21.2	17	19.1	22.2	22.1	19.8	21.1	21	17.6	21.4	20.3	18.6	21	19.7	20.8	18.1	19.4	18.6	18.3	20.3	19.2	21.3	21	22.5	19.7	20.9	21.8	19.8	20.4	20.8	20.4	19.6	21.2	20	21	19	19.1	20.6	20.7	22.9	19.2	18.9	18.6	22.8	20.6	20.5	21.4	22	21.5	19.8	20.8;      % cm, snout-to-vent length at f
%           65	90	373	200	230	302	333	386	330	380	365	307	332	381	400	260	410	250	360	280	250	345	260	381	353	275	248	291	35	170	415	345	253	233	320	422	403	235	355	340	399	295	80	281	270	293	286	365	333	479	214	398.2768003	94.71499397	451.9870374	44.49329547	60.39155219	361.7359356	555.6298524	424.1412471	345.4373917	484.5600464	472.6410518	453.9293598	365.9792089	389.3665848	429.8231421	407.0143556	572.8924603	454.3888071	471.9904648	309.7587006	311.9524098	516.9407698	274.4282561	255.382112	392.2479404	355.3906588	331.4227318	432.0583592	318.4322903	413.7943101	241.8492916	298.7064284	459.8218425	327.4875088	365.6716544	311.8393314	478.5660079	362.4274557	372.9721825	323.3258161	438.3600411	380.7200929	431.9238788	424.4893538	437.3303383	322.3331475	424.6677409	459.5401734	271.28183	429.0952619	416.6934263	294.1244856	507.695046	414.084911	464.8715518	283.0391092	409.8770488	411.8901471	328.5082575	457.51482	303.120508	502.7509423	472.1687781	303.8661128	408.0248439	524.666256	546.2863414	335.3081683	316.3900775	394.5400354	382.7968287	375.6735279	370.1886879	314.5993985	481.0352065	478.8694428	489.1060876	513.6119826	393.8940644	313.5764805	414.7345514	183.299972	510.4413959	137.0805976	373.0796502	353.7831899	367.6553603	318.0673283	362.0315257	468.2897108	264.6651809	298.6139477	459.7980138	322.0007581	345.9005526	293.1184384	399.0699531	369.0164639	401.4077222	359.9037907	273.9755696	448.7681424	408.228808	506.4164748	357.3402198	526.5013286	464.0220901	378.1368146	454.3114859	439.0932886	403.5211364	320.2618897	330.3673197	329.2207667	282.6140025	360.1728125	453.0190401	379.4809196	394.8919226	313.9680165	376.4251979	285.8389897	321.0658583	442.9783079	355.2318676	431.1881531	339.3953095	326.0891787	453.8929381	447.1340824	506.6097842	351.2411051	244.7009971	145.7289545	294.6235065	407.6494246	319.3764274	353.5769846	383.4775041	344.4793886	387.1397755	271.1255978	378.0049387	230.7167046	386.1007268	435.2370648	383.7974347	193.4734524	445.2447993	397.3746572	366.2407633	398.0852858	299.6348359	148.6837415	507.0399641	339.5104439	361.2341354	268.0374092	523.0437867	466.9280415	282.0354344	502.342174	322.1741301	450.3657199	302.9848384	477.6305169	302.2115392	205.1991317	332.4380191	280.9288177	332.7611125	354.8112572	309.7770596	90.40343792	436.5234272	382.5687823	363.3618156	324.9546082	370.0150272	283.3418121	443.0040392	416.1574151	168.0579004	463.4539246	409.8754286	426.3235745	402.7725835	208.8889547	528.7807713	390.7710423	221.0273083	311.5410848	371.5806693	410.0360551	456.3549184	449.7663551	262.290825	402.2577456	154.4130549	335.8595518	327.8357332	422.1826952	453.216962	146.795177	282.098408	314.4618968	183.5067691	491.7536383	335.0553699	317.1177928	362.9812007	375.6282233	175.3841914	446.4686518	485.7297512	361.4453618	453.4893419	624.4626892	321.4843593	363.1128037	259.9401518	311.7730869	258.5320161	332.5391737	286.8104628	433.3360988	314.4855401	484.5275005	90.61442972	475.2044029	340.6449384	572.6037289	371.6383621	509.074838	433.4652619	416.2191335	578.5000694	329.6918098	369.6125047	510.857973	322.1776146	473.7893091	375.6393146	259.249014	428.5559783	351.4127953	421.525585	373.5612968	106.9309968	157.949652	126.1677596	516.1210248	351.5840779	419.0939259	376.114855	80.93114091	449.6874511	442.9727911	120.4200319	356.0135329	469.4219217	416.2477741	438.3391461	162.3940391	448.2240888	169.1654692	417.0250179	323.7728867	432.3557722	353.9378682	425.4049139	274.9218166	377.1687201	402.2826825	412.8742253	394.3094775	387.5685448	504.5985303	312.520918	323.4048063	289.0408792	513.6586505	387.2265527	381.6346738	315.8448851	313.5962073	411.6822928	123.594988	368.1393563	543.7854325	423.5328341	299.7704622	401.4536382	502.7697649	341.8472087	302.8708877	387.7375244	265	345	379	383	337	373	340	383	231	378	252	427	462.249805	398.8305574	437.6827061	498.0748382	413.0111119	386.5921129	329.1022203	299.4216491	373.6906731	447.8031046	431.503361	473.2623146	448.0601106	395.8588054	398.1734435	391.757008	417.2108977	382.0101747	366.7758432	335.7580617	371.8908476	438.2575973	464.0605714	361.5099565	412.6289783	386.474554	367.8106618	351.5675798	329.8680528	556.3686453	334.6512619	389.4196221	412.0714909	299.7167275	317.598217	385.4873603	471.3329204	519.2761605	319.7067477	443.1846893	493.5868144	296.9516232	329.1202738	518.7135657	531.7963975	342.1298262	471.8729384	381.3038943	276.7479118	424.0904536	435.564723	312.0657809	409.4216888	423.6094263	443.8430358	319.7883241	401.556738	329.0100241	318.9379357	460.4510748	360.4789624	418.1451907	394.9763378	492.8539588	398.2924494	415.4164985	505.4808207	240	352	382.7614783	384.9369553	372.0438645	452.7225476	437.5456264	390.8140279	328.8587057	251	363	325	520.4976693	321.6552966	326.7917653	314.7227486	579.0343394	438.5255282	424.3551011	427.4183656	496.6667859	475.753222	366.9412016	449.6498942]';   % g, wet weight at f and T
%units.LW = {'cm', 'g'};     label.LW = {'time since birth', 'wet weight'};  bibkey.LW = 'Dawbin_1982';

%  data.TO = [ ... temp (C), O2 consumption (ml/h.gwet) of 18.90108 g wet weight (5.67 g dry), based on Scott's data
% 5   0.016422
% 12	0.002366757/12*60
% 20	0.007695684/12*60
% 24	0.011011122/12*60
% 27	0.020467552/12*60
% 29	0.028250338/12*60
% 30	0.032857636/12*60
% ];
%units.TO = {'mlO2/gwet/h', 'C'};     label.TO = {'O2 consumption rate', 'temperature'};  bibkey.TO = 'Scott_unpub; Cartland_Grimmond_1994_5C_medium';
%ID 139; wet weight 362.4g
data.TO = [ ... 
%data.TO_139 = [ ... 
15	0.002085919
21	0.006957514
25	0.01929228
31	0.03080439%];
%units.TO_139 = {'C', 'ml O2/g/h'};     label.TO_139 = {'temp', 'O2 consumption'};  bibkey.TO_139 = 'Arnall et al. 2015';

% ID 204; wet weight 288.2g
%data.TO_204 = [ ... 
15	0.002556972
21	0.007311692
25	0.01694881
31	0.02668169%];
%units.TO_204 = {'C', 'ml O2/g/h'};     label.TO_204 = {'temp', 'O2 consumption'};  bibkey.TO_204 = 'Arnall et al. 2015';

% ID 269; wet weight 297.2g
%data.TO_269 = [ ... 
15	0.001478324
21	0.005955151
25	0.01790451
31	0.03496208%];
%units.TO_269 = {'C', 'ml O2/g/h'};     label.TO_269 = {'temp', 'O2 consumption'};  bibkey.TO_269 = 'Arnall et al. 2015';


% ID 354; wet weight 277.3g
%data.TO_354 = [ ... 
15	0.001204873
21	0.006027985
25	0.01742385
31	0.02480775%];
%units.TO_354 = {'C', 'ml O2/g/h'};     label.TO_354 = {'temp', 'O2 consumption'};  bibkey.TO_354 = 'Arnall et al. 2015';


% ID 524; wet weight 322.0g
%data.TO_524 = [ ... 
15	0.001312231
21	0.005149532
25	0.01897694
31	0.02305341%];
%units.TO_524 = {'C', 'ml O2/g/h'};     label.TO_524 = {'temp', 'O2 consumption'};  bibkey.TO_524 = 'Arnall et al. 2015';


% ID 690; wet weight 293.5g
%data.TO_690 = [ ... 
15	0.002410638
21	0.008921721
25	0.01756489
31	0.02747758%];
%units.TO_690 = {'C', 'ml O2/g/h'};     label.TO_690 = {'temp', 'O2 consumption'};  bibkey.TO_690 = 'Arnall et al. 2015';


% ID 716; wet weight 269.3g
%data.TO_716 = [ ... 
15	0.001236561
21	0.006155903
25	0.01767426
31	0.02939617%];
%units.TO_716 = {'C', 'ml O2/g/h'};     label.TO_716 = {'temp', 'O2 consumption'};  bibkey.TO_716 = 'Arnall et al. 2015';


% ID Z1; wet weight 308.7g
%data.TO_Z1 = [ ... 
15	0.002687078
21	0.005811912
25	0.01850031
31	0.03345558];
units.TO_Z1 = {'C', 'ml O2/g/h'};     label.TO_Z1 = {'temp', 'O2 consumption'};  bibkey.TO_Z1 = 'Arnall et al. 2015';
units.TO = {'C', 'ml O2/g/h'};     label.TO = {'temp', 'O2 consumption'};  bibkey.TO = 'Arnall et al. 2015';

%% set weights for all real data
weight = setweights(data, []);

%% overwriting weights (remove these remarks after editing the file)
% the weights were set automatically with the function setweigths,
% if one wants to ovewrite one of the weights it should always present an explanation example:
%
% zero-variate data:
% weight.Wdi = 100 * weight.Wdi; % Much more confidence in the ultimate dry
%                                % weight than the other data points
 weight.Ri = 100*weight.Ri;
 %weight.Wdb = 50*weight.Wdb;
%weight.Wdp = 10*weight.Wdp;
%weight.Wdi = 100*weight.Wdi;
%weight.ap = 10*weight.ap;
 weight.ab = 10*weight.ab;
 %weight.Li = 50*weight.Li;


% uni-variate data: 
 %weight.TO = 0 * weight.TO;
 %weight.LW = 5 * weight.LW;
 %weight.tL = 5 * weight.tL;

%% set pseudodata and respective weights
% (pseudo data are in data.psd and weights are in weight.psd)
[data, units, label, weight] = addpseudodata(data, units, label, weight);

%% overwriting pseudodata and respective weights (remove these remarks after editing the file)
% the pseudodata and respective weights were set automatically with the function setpseudodata
% if one wants to ovewrite one of the values it should always present an explanation
% example:
% data.psd.p_M = 1000;                    % my_pet belongs to a group with high somatic maint 
% weight.psd.kap = 10 * weight.psd.kap;   % I need to give this pseudo data a higher weight

%% pack data and txt_data for output
data.weight = weight;
data.temp = temp;
txt_data.units = units;
txt_data.label = label;
txt_data.bibkey = bibkey;

%% References
  bibkey = 'Wiki'; type = 'Misc'; bib = ...
  'URL = {https://en.wikipedia.org/wiki/Tuatara}';   % replace my_pet by latin species name
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Kooy2010'; type = 'Book'; bib = [ ...  % used in setting of chemical parameters and pseudodata
  'author = {Kooijman, S.A.L.M.}, ' ...
  'year = {2010}, ' ...
  'title  = {Dynamic Energy Budget theory for metabolic organisation}, ' ...
  'publisher = {Cambridge Univ. Press, Cambridge}, ' ...
  'pages = {Table 4.2 (page 150), 8.1 (page 300)}, ' ...
  'URL = {http://www.bio.vu.nl/thb/research/bib/Kooy2010.html}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Jarvie_unpub'; type = 'Thesis'; bib = [ ... % meant as example; replace this and further bib entries
  'author = {Jarvie, S. and Cree, A.}, ' ...
  'year = {2015}, ' ...
  'title = {TBA}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);
  %
  bibkey = 'Anon2015'; type = 'Misc'; bib = [ ...
  'author = {Anonymous}, ' ...
  'year = {2015}, ' ...
  'URL = {http://www.fishbase.org/summary/Rhincodon-typus.html}'];
  eval(['metadata.biblist.' bibkey, '= ''@', type, '{', bibkey, ', ' bib, '}'';']);

%% Facts
% * Standard model with egg (not foetal) development and no acceleration
  
%% Discussion points
pt1 = 'Kearney: there is a github repository for this project git/mrke/tuatara';
pt2 = 'Kearney: TA was estimated from Yuni''s unpublished data on sprint speed (/sprint speed/sprint_speed_N_occelatus_Yuni.csv), using script /sprint speed/TA from sprint speed.R';
pt3 = 'Jarvie: metabolic rates were extracted from Jarvie''s measurements of metabolic rate at six temperatures (12, 20, 24, 27, 29 and 30C). We only used metabolic rate for animals presumed to be females, due to the temperatures that they were incubated at. We also used a metabolic rate measurement at 5C, for medium-sized animals, from Cartland Grimmond 1994';
pt4 = 'Kearney: Temperatures for ';     
metadata.discussion = {pt1; pt2; pt3; pt4};
