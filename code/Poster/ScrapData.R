library(SocialMediaLab)
myApiKey <- "AIzaSyAgCkX9cZrRKFBHpJJfuqIBboektfCnIHk" # DO NOT PUBLIZE THIS KEY for the users eyes only
apiKeyYoutube <- AuthenticateWithYoutubeAPI(apiKeyYoutube=myApiKey)

### data v1:v28 are videos that I collected independently
datav1 <- CollectDataYoutube('ENocyN7Blk4',apiKeyYoutube) # title: why i quit the military, date: 11/19/2012, user: raheem miller, # of comments: 1583, # of views: 150157
datav2 <- CollectDataYoutube('GZixH7YBYso',apiKeyYoutube) # title: the us military's evolving transgender policy, date: 03/02/2015, user: seeker daily, # of comments: 804, # of views: 50312
datav3 <- CollectDataYoutube('VwwMF6biCJU', apiKeyYoutube) # title: kill everybody american soldieder exposes US policy in Iraq, date: 04/26/2007, user: yoryevrah, # of comments: 11503, # of views: 1196109
datav4 <- CollectDataYoutube('cVChYjrDRIM', apiKeyYoutube) # title: army's new hair rules awkward or straight up racist, date: 04/03/2014, user: the young turks, # of comments:2104 , # of views: 95874
datav5 <- CollectDataYoutube('NkWwZ9ZtPEI', apiKeyYoutube) # title:now,after PTSD from a soldier's pov, date: 12/02/2010, user: blue three productions, # of comments: 514, # of views: 318645
datav6 <- CollectDataYoutube('ap8HBVFNNho', apiKeyYoutube) # title: us army basic training, date: 08/03/2011, user: installationmgt, # of comments: 6699, # of views: 4611812
datav7 <- CollectDataYoutube('6C6aH8hHuVk', apiKeyYoutube) # title: us soldiers talk about the taliban, date: 07/07/2009, user: fallenussoldiers, # of comments: 1394, # of views: 195135
datav8 <- CollectDataYoutube('lkhaDCzplsI', apiKeyYoutube) # title: women in the military a soldier's perspective, date: 08/29/2013, user: mrrtkwe, # of comments: 406, # of views: 41353
datav9 <- CollectDataYoutube('yy27jAcsrWY', apiKeyYoutube) # title: abuse of women in the us military/army/navy PFC lavena johnson the silent truth, date: 03/09/2012, user: iseenews, # of comments: 1657, # of views: 639350
datav10 <- CollectDataYoutube('0Zu97p-0Sfs', apiKeyYoutube) # title: us army in afghanistan fierce shooting real combat, date: 12/17/2013, user: crazy war, # of comments: 1009, # of views: 1436821
datav11 <- CollectDataYoutube('KYlNbfXit8g', apiKeyYoutube) # title:the army hates single soldiers, date:08/25/2013, user:aschmetzer42, # of comments: 409, # of views:31911 
datav12 <- CollectDataYoutube('iTHIsscwZjg', apiKeyYoutube) # title: army basic combat training (fort sill), date: 12/14/2013, user: danny brandt, # of comments: 749, # of views: 441344
datav13 <- CollectDataYoutube('ANkdo5StzZA', apiKeyYoutube) # title: 5 things you don't know US army, date: 06/19/2015, user: military.com, # of comments: 2204, # of views: 488553
datav14 <- CollectDataYoutube('4UThH3nzoUE', apiKeyYoutube) # may not be the best video topic; title: us army using its best weapons during desert attach m1 abrams/ah-64/f-15 etc in action, date: 08/16/2015, user: daily military defense and archive, # of comments: 2695, # of views: 5160320
datav15 <- CollectDataYoutube('vx-LWMqozsE', apiKeyYoutube) # title: why did i leave the military, date: 06/21/2011, user: varmit coyote's backup channel, # of comments: 2231, # of views: 148501
datav16 <- CollectDataYoutube('aCQramsY3Us', apiKeyYoutube) # title: Meet the First Transgender Soldier in the U.S. Military, date: 04/25/2015, user: Willie Talk, # of comments: 881, # of views: 161945
datav17 <- CollectDataYoutube('zN7VSeja1dw', apiKeyYoutube) # title: Transgender, at War and in Love, date: 06/10/2015, user: The New York Times, # of comments: 1,204, # of views: 291905 
datav18 <- CollectDataYoutube('Yc5O6Y-Yeyw', apiKeyYoutube) # title: U.S. Army Uniform Weight of Warfare, date: 11/17/2009, user: Justin Merriman, # of comments: 1493, # of views: 540585
datav19 <- CollectDataYoutube('oGHyBJoMOGE', apiKeyYoutube) # title: Funny US Army Soldiers!, date: 12/10/2009, user: mcluvinoj, # of comments: 208, # of views: 163753
datav20 <- CollectDataYoutube('05bnplpTSq8', apiKeyYoutube) # title: American Soldier Breaksdown, date:06/29/2010, user:Dattucha, # of comments: 4887, # of views: 1254692   
datav21 <- CollectDataYoutube('5xqcrB6wyzo', apiKeyYoutube) # title: US Soldiers Crying After Ambush, date: 04/26/2011, user: CounterTerrorismUnit's channel, # of comments: 9970, # of views: 1103904  
datav22 <- CollectDataYoutube('WPatV7ZwhaY', apiKeyYoutube) # title: Very Sad Iraq USA Army Dedication Video, date: 11/02/2006, user: RealMuscleGamer, # of comments: 3018, # of views: 960327  
datav23 <- CollectDataYoutube('nakCqUmJn98', apiKeyYoutube) # title: U.S. Soldier Who Murdered Civilians, date: 06/08/2015, user: Secular Talk, # of comments: 1336, # of views: 55512
datav24 <- CollectDataYoutube('eZ6oS5dUT30', apiKeyYoutube) # title: Dogs Welcoming Soliders Home Compilation 2014, date: 10/27/2014, user: funnyplox, # of comments: 1012, # of views: 1443847
datav25 <- CollectDataYoutube('uUv9SXShTog', apiKeyYoutube) # title: Disrespect in the US Army, date: 06/16/2016, user: Drip46, # of comments: 418, # of views: 7093  
datav26 <- CollectDataYoutube('8b2X3ZdZrI4', apiKeyYoutube) # title: Army Ranger Suicide Before 9th Deployment To Iraq/Afghanistan, date: 08/15/2011, user: The Young Turks # of comments: 2444 # of views: 229121
datav27 <- CollectDataYoutube('FeMOS-Z8yok', apiKeyYoutube) # title: Army $$ for Anti-Suicide Nasal Spray, date: 08/21/2012, user: Sourcefed, # of comments: 2468, # of views: 168510
datav28 <- CollectDataYoutube('0_StCzStBy0', apiKeyYoutube) # title: Purple Heart's Final Beat A Soldier's Suicide Story, date: 04/30/2009, user: Blake Kleiner, # of comments: 496, # of views: 99526

######### Following chunks of code are organized by topics chosen by the policy group

### education 
educv1 <- CollectDataYoutube('nID_vCiWhTI', apiKeyYoutube) # title: How GI Bills Work (US Military Educational benefits), date: 07/08/2013, user: mikey16piet, # of comments: 45, # of views: 13198
educv2 <- CollectDataYoutube('qdExqYKP2gc', apiKeyYoutube) # title: Montgomery GI Bill & Post 9/11 GI Bill, date: 11/10/2015, user: Kyle Gott, # of comments: 114, # of views: 10340
educv3 <- CollectDataYoutube('dIvn5nTDMCs', apiKeyYoutube) # title: MILITARY BENEFITS | MILITARY PAY & EDUCATION, date: 12/08/2015, user: ARCHIEzzle, # of comments: 18, # of views: 5516
educv4 <- CollectDataYoutube('At7K8KLaxcU', apiKeyYoutube) # title: Join the Army Reserves to Help Pay for College!!, date: 02/18/2016, user: Strength Over Benches (SOB), # of comments: 32, # of views: 5382
educv5 <- CollectDataYoutube('aQUNQEXbGvY', apiKeyYoutube) # title: My experience using the POST-911 GI BILL (Update Vlog), date: 12/20/2015, user: JTsuits, # of comments: 72, # of views: 3085

### health 
healthv1 <- CollectDataYoutube('kZNscepy35s',apiKeyYoutube) # title: US Army Infantryman talks about PTSD and Psychiatric Drugging, date: 02/25/2014, user: CCHRflorida, # of comments: 31, # of views: 9041
healthv2 <- CollectDataYoutube('bJj4f3q3RgY', apiKeyYoutube) # title: US army program focuses on soldiers' mental health, date: 07/06/2011, user: press tv, # of comments: 6, # of views: 428
healthv3 <- CollectDataYoutube('yEpAagxoClA', apiKeyYoutube) # title: More Military Suicides than Combat Deaths US Soldiers & Suicide - PTSD & Psychiatric Drugs, date: 09/01/2014, user: PsycheTruth, # of comments: 53, # of views: 10060
healthv4 <- CollectDataYoutube('4DF5caucKjI', apiKeyYoutube) # title: Soldier talks about his struggle with depression and PTSD, date: 08/25/2013, user: MiliSource, # of comments: 273, # of views: 52087
healthv5 <- CollectDataYoutube('seMzGydVJHE', apiKeyYoutube) # title: Post Traumatic Combat Stress, date: 07/07/2014, user: Thirstypioneer, # of comments: 93, # of views: 33113

### incentives 
incv1 <- CollectDataYoutube('q3N80TVSoHY', apiKeyYoutube) # title: Military Funeral, Medal of Honor Recipient, Ret. US Army Col. James L. Stone Jr., date: 11/25/2012, user: USFallen.org, # of comments: 130, # of views: 213161
incv2 <- CollectDataYoutube('7GWVSV1vl6A', apiKeyYoutube) # title: President Obama awards Captain William Swenson, U.S. Army, the Medal of Honor, date: 10/15/2013, user: The White House, # of comments: 236, # of views: 96568
incv3 <- CollectDataYoutube('utZhdqKPsA8', apiKeyYoutube) # title: Does The U.S. Military Have A Diversity Problem?, date: 01/25/2016, user: Seeker Daily, # of comments: 1840, # of views: 90720
incv4 <- CollectDataYoutube('dJgZTUAR1lU', apiKeyYoutube) # title: Medal of Honor Ceremony For 24 Army Veterans, date: 03/19/2014, user: USA Patriotism!, # of comments: 67, # of views: 58277
incv5 <- CollectDataYoutube('c8j2Gdtb4UU', apiKeyYoutube) # title: A Look at Army Benefits, date: 06/02/2010, user: GOARMY.com, # of comments: 34, # of views: 50858

### privacy
priv1 <- CollectDataYoutube('c672ZmLpf0w', apiKeyYoutube) # title: 'Human error': No criminal charges for US army personnel over Kunduz hospital bombing, date: 04/30/2016, user: RT, # of comments: 450, # of views: 8940
priv2 <- CollectDataYoutube('5f13jrjaekY', apiKeyYoutube) # title: Stereophonics - Hurry Up And Wait, date: 04/12/2010, user: StereophonicsVEVO, # of comments: 81, # of views: 350698
priv3 <- CollectDataYoutube('Xv8Nd64n9QU', apiKeyYoutube) # title: STUPID SELFIE! Soldier Posts Pic Of Herself Avoiding Flag Salute, Ignites Online FIRESTORM!!, date: 02/27/2014, user: Hezakya Madison, # of comments: 613, # of views: 72696

### quality of life
qolv1 <- CollectDataYoutube('S5iuPHV_ILw', apiKeyYoutube) # title: Thinking About Joining the Army? - Army Food and Accommodations, date: 06/20/2012, user: themop11, # of comments: 118, # of views: 64026
qolv2 <- CollectDataYoutube('958SFZN7EFI', apiKeyYoutube) # title: US Army: What its like to serve on active duty - Army Strong [PROMO], date: 10/30/2008, user: US Army in Korea, # of comments: 85, # of views: 38063
qolv3 <- CollectDataYoutube('4f9wPb6sqx0', apiKeyYoutube) # title: SENDING LETTERS IN THE ARMY, date: 06/08/2016, user: ARCHIEzzle, # of comments: 79, # of views: 1367
qolv4 <- CollectDataYoutube('rbDoBBTHZtA', apiKeyYoutube) # title: Don't Fall Asleep in the Military, date: 01/30/2015, user: Uniform Stories, # of comments: 982, # of views: 2,595,807
qolv5 <- CollectDataYoutube('yDy9XdyZIaY', apiKeyYoutube) # title: CNN: Soldiers life on a military base, date: 09/29/2010, user: CNN, # of comments: 52, # of views: 81259

### requirements
reqv1 <- CollectDataYoutube('Egp-_KyGC5U', apiKeyYoutube) # title: Requirements to Join the Army, date: 06/01/2012, user: Military Spot, # of comments: 160, # of views: 68421
reqv2 <- CollectDataYoutube('w1d3Rj2dEBM', apiKeyYoutube) # title: U.S. Army Physical Fitness Test, date: 06/26/2014, user: AiirSource Military, # of comments: 122, # of views: 97652
reqv3 <- CollectDataYoutube('uw9aZ-plC8c', apiKeyYoutube) # title: HOW OLD IS TOO OLD IN THE ARMY? ASK A SOLDIER # 6, date: 03/30/2016, user: ARCHIEzzle, # of comments: 77, # of views: 4915
reqv4 <- CollectDataYoutube('XZfQd4m29uA', apiKeyYoutube) # title: Army Ranger Requirements - How to Become an Army Ranger., date: 08/07/2015, user: Foxtrot Alpha, # of comments: 259, # of views: 182846
reqv5 <- CollectDataYoutube('1lbwjVXsGx8', apiKeyYoutube) # title: U.S. Army tests whether women can make the cut in combat, date: 04/08/2014, user: CBS Evening News, # of comments: 214, # of views: 25732

### retirement
retv1  <- CollectDataYoutube('N6rjCvgRkq0', apiKeyYoutube) # title: America's Deported Veterans La Frontera, date: 07/03/2014, user: VICE news, # of comments: 1943, # of views: 256935
retv2 <- CollectDataYoutube('vr1zMBN0g0w', apiKeyYoutube) # title: America's Veteran Crisis Abandoned at Home, date: 06/30/2014, user: VICE news, # of comments: 2228, # of views: 344228
retv3 <- CollectDataYoutube('aopOQFLk1fY', apiKeyYoutube) # title: U.S. retirement benefits to be cut?, date: 05/08/2010, user: TheRealNews, # of comments: 161, # of views: 10884
retv4 <- CollectDataYoutube('GzeV1XREpgo', apiKeyYoutube) # title: Senate Passed Budget Bill Cutting Military Retirement Benefits - The Kelly File, date: 12/18/2013, user: Mass Tea Party - Wake Up America!, # of comments: 28, # of views: 1780
retv5 <- CollectDataYoutube('Q9nsXBJi4RQ', apiKeyYoutube) # title: New US Military Retirement Plan, date: 02/27/2016, user:Army NCO Support, # of comments: 6, # of views: 4468
retv6 <- CollectDataYoutube('_8rbHwMXMT8', apiKeyYoutube) # title: Advice for those considering joining the military, date: 09/04/2007, user: subjectandobject, # of comments: 2212, # of views: 1047233

### sexual assault
womenv1 <- CollectDataYoutube('lkhaDCzplsI', apiKeyYoutube) # title: women in the military a soldier's perspective, date: 08/29/2013, user: mrrtkwe, # of comments: 406, # of views: 41353
womenv2 <- CollectDataYoutube('yy27jAcsrWY', apiKeyYoutube) # title: abuse of women in the us military/army/navy PFC lavena johnson the silent truth, date: 03/09/2012, user: iseenews, # of comments: 1657, # of views: 639350
womenv3 <- CollectDataYoutube('9PGPWp-4kvQ',apiKeyYoutube) # title: how bad is sexual abuse in the US military, date: 07/16/2015, user: seeker daily, # of comments: 1105, # of views: 98161
womenv4 <- CollectDataYoutube('IAKiCdD0hkE', apiKeyYoutube) # title: The Invisible War: New Film Exposes Rape, Sexual Assault Epidemic in U.S. Military, date: 01/30/2012, user: Democracy Now!, # of comments: 483, # of views: 107323
womenv5 <- CollectDataYoutube('1zpj9XoVFoI', apiKeyYoutube) # title: The Invisible War, date: 10/01/2012, user: Reveal, # of comments: 146, # of views: 88221
womenv6 <- CollectDataYoutube('cD5MvyQu24c', apiKeyYoutube) # title: Effects of Sexual Assault/Sexual Harassment on the Army Profession, date: 06/10/2013, user: Center for the Army Profession and Ethic (CAPE), # of comments: 100, # of views: 126819
womenv7 <- CollectDataYoutube('pbKiOibGFkE', apiKeyYoutube) # title: The U.S. Military Has A Major Sexual-Assault Problem, date: 05/19/2015, user: AJ+, # of comments: 100, # of views: 6155
womenv8 <- CollectDataYoutube('gso5BiuDPto', apiKeyYoutube) # title: High Sexual Assault Numbers in Military Prompt McCain to Advise Women Not to Join, date: 06/04/2013, user: PeoplesUnderground, # of comments: 52, # of views: 4069
womenv9 <- CollectDataYoutube('3fBaFQk6aE0', apiKeyYoutube) # title: The Invisible War Official Trailer #1 - Kirby Dick Movie (2012) HD, date: 12/01/2011, user: Movieclips Trailer, # of comments: 778, # of views: 516957
womenv10 <- CollectDataYoutube('Za8cY_zd9Qw', apiKeyYoutube) # title: 'Invisible War' Shines Light on Rape in the Military, date: 02/22/2013, user: ABC News, # of comments: 197, # of views: 59049

### transgender
transv1 <- CollectDataYoutube('HdzU0DZEzck', apiKeyYoutube) # title: US Military Ends Transgender Ban, date: 07/02/2016, user: Secular Talk, # of comments: 1116, # of views: 32556
transv2 <- CollectDataYoutube('3L5yRBRYjeY', apiKeyYoutube) # title: Meet The First Openly Trans Man In The U.S. Military, date: 07/29/2015, user: Raymond Braun, # of comments: 450, # of views: 172802 
transv3 <- CollectDataYoutube('o6faZjRW2xY', apiKeyYoutube) # title: Military To End Ban On Transgender Soldiers, date: 07/18/2015, user: the young turks, # of comments: 1387, # of views: 53160
