{-# LANGUAGE OverloadedStrings #-}

module Data.Digest.Human.BsAtron (humanHashBS) where

import Data.Foldable
import Data.Hashable
import Data.Text     (Text)
import System.Random

import Data.Digest.Human.Hash

-- | Build a human hash using the bsAtron
humanHashBS :: (Foldable t, Hashable a) => Int -> t a -> [Text]
humanHashBS c = fixup . humanHashBy (bsAtron posNegGuide c) c
  where
    posNegGuide = take c $ randoms $ mkStdGen c
    fixup []             = []
    fixup h@(_:[])       = h
    fixup hhs@(h:hs)
        | c `mod` 2 == 0 = hhs
        | otherwise      = h : (last hs) : init hs

-- | Generate human hash bullshit
bsAtron :: [Int] -> Int -> Int -> Int -> Text
bsAtron rs c i
    | last           = f places
    | i `mod` 2 /= 0 = f nouns
    | r `mod` 2 == 0 = f positive
    | r `mod` 2 /= 0 = f negative
  where
    last = c `mod` 2 /= 0 && (i + 1) == c
    r    = rs !! i
    f w  = (w !!) . (flip mod (length w))

nouns :: [Text]
nouns = ["glacier","proxy","torture","profit","moraine","temp","recruit","cruise","price","peninsula","hike","overboard","cruel","discount","spoils","airport","explore","financial","wholesaler","tramway","creek","cargo","watershed","tributary","rallentando","lake","sunscreen","recruiter","demisemiquaver","tonic","tent","treasury","sandbar","knife","tourist","wander","cabin","currency","rest","grave","embark","mates","password","motel","arms","museum","opportunity","beach","fear","skiff","leave","land","accounts","theme","pillage","maroon","laptop","assault","playa","gap","profitable","loess","parkway","syncopation","elephant","hull","network","return","ride","subdominant","atoll","weekend","chasm","headland","dale","advertise","shoreline","heist","lore","sextant","deflation","bear","invoice","vandalize","thievery","departure","modulation","flag","coastline","fifth","merchant","lane","copyright","umbrella","river","sostenuto","intern","production","download","espressivo","low","ferry","behead","abandon","ravine","overpass","sharp","acrobat","pond","arch","hire","fleet","interval","ledge","maggot","pitch","compass","employer","trend","plunder","market","trainee","cape","ornament","buy","arpeggio","drift","cirque","furioso","mainframe","monopoly","nautical","cape","plane","stagehand","tranquillo","prestissimo","retail","agreement","unicycle","venture","major","executive","payment","refund","reservations","bandanna","outdoors","restaurant","segno","scurvy","swimsuit","landform","intonation","conquest","tides","fine","carousel","train","vehicle","scree","delta","mayhem","leave","danger","risk","popcorn","legend","vacancy","livestock","customer","overhang","juggler","brook","getaway","thruway","incentive","matiner","scale","point","crescendo","jet","bus","takeoff","waterpark","prairie","zoo","holiday","octave","keelhaul","passage","scarp","dolce","vanquish","figures","sell","online","schedule","mast","freight","band","rigging","dismiss","ship","go","pesante","beltway","fermata","waterfall","horizon","tram","duty","spit","games","capsize","voyage","downtime","unlawful","lucre","bridge","tour","magician","relax","accent","gully","plateau","ransack","reckoning","curse","interchange","stock","rudder","privateer","president","luggage","canyon","flight","shoal","topography","scalawag","pay","swamp","ads","scar","cavern","gulf","coda","mutiny","ocean","menace","speedway","shore","timbre","terms","success","bond","trick","molto","hostile","bay","dividend","cliff","net","brawl","hollow","corpse","retailer","yacht","show","highway","corsair","staff","commission","currency","musket","sack","lion","affordable","keepsake","dog","narrows","minor","road","promontory","allegretto","fly","net","quartermaster","harmonics","presentation","violent","collateral","competition","knapsack","corporation","résumé","transaction","superhighway","salary","crook","spring","upload","adagio","withdraw","leotard","charge","cantabile","lento","seashore","vale","flag","tax","cannon","gibbet","coast","performer","treasure","tundra","symphony","meander","ahoy","walk","dissonance","ruffian","economics","rations","fair","equator","organization","landlubber","terrace","typeface","robber","hours","slur","distribution","lease","adventure","superintendent","souvenir","hijack","framework","parrot","inflation","ccountant","sail","dishonest","hiatus","guidebook","translate","crotchet","tempo","intersection","jazz","efficiency","walkway","lbox","natural","attack","sailor","motorway","roam","arbitration","leisure","lake","spreadsheet","forte","coins","vessel","notebook","mound","basin","way","vacation","counterpoint","trampoline","waterfall","shore","bicycle","illegal","violence","pianissimo","gulch","meeting","drive","negotiate","boatswain","tote","company","journey","andantino","valley","scherzando","isthmus","flew","transportation","asea","employee","workspace","hold","fearsome","bounty","trail","motherboard","bandolier","client","investment","badlands","trail","haul","harmony","geyser","dell","prizes","map","pentatonic","magic","fax","ticket","blues","thug","armada","quest","foreign","swim","greed","wastebasket","notice","cave","attractions","work","ocean","star","inn","hoop","staff","sailing","seller","visit","cave","workroom","owner","manage","invest","reef","destination","chimney","quaver","island","caldera","truce","stockholder","rum","music","opera","open","bayou","cost","buyer","trade-off","volume","entertain","lookout","coast","tightrope","diatonic","triplets","memo","tariff","gorge","ridge","gunpowder","strike","horse","scenery","captain","supervisor","tip","triad","theory","upgrade","signature","ship","unscrupulous","bluff","room","hostel","ashore","diversify","terrain","entertainer","desert","swashbuckling","seaweed","animals","semiquaver","measure","trunk","discount","sound","duties","car","realm","fissure","cutlass","bandit","melody","occupation","backpack","niche","partner","sea","ringmaster","riches","office","raid","headwaters","sights","lodge","candy","archipelago","tent","economical","excursion","bike","explore","dynamics","spectator","factory","malaria","pistol","contract","database","pass","cloverleaf","accelerando","passage","navigate","capital","avenue","tenuto","commerce","finance","management","legato","camping","swing","peak","predatory","rob","shore","facility","poster","labor","director","credit","gold","seas","import","mainland","merchant","lash","gross","anchor","estuary","fire","slope","dominant","plan","keel","lucrative","escarpment","safari","bicycle","place","lodging","rest","goods","ring","airplane","worker","exhibition","job","crew","swag","hillside","festival","ritard","commodity","villain","dagger","applaud","sequence","parley","waybill","bass","transposition","rank","lakebed","animato","latitude","sail","accruals","earring","highland","accounting","fun","bonus","treachery","liability","sea","briefcase","beach","thoroughfare","somersault","position","crag","quarters","brutal","doubloon","byway","sempre","octatonic","tiger","order","bowl","resort","sabotage","infamous","travel","clause","boat","riverbed","dune","cashier","iceberg","stave","stay","perk","source","retire","island","criminal","col","quit","weapons","causeway","ticket","balloons","vicious","inventory","high","key","shipment","longitude","close","relaxation","course","target","cargo","cove","tablature","decrescendo","estimate","tritone","costume","yield","polyphony","barrel","fault","calculate","warranty","fortune","laborer","overtone","sonata","proposal","pack","portfolio","wetland","merchandise","trade","surf","trill","trip","lawless","arroyo","religioso","audience","lowland","overlook","clown","shallows","maze","foothills","knoll","cataract","galleon","dome","divide","giant","treble","jewels","shop","letterhead","simile","agenda","supply","repeat","summit","piano","allegro","policy","quarter","music","plain","tableland","troupe","festive","marsh","rise","ria","butte","statement","demand","mouth","escape","budget","stilts","growth","expenses","username","employment","flat","expressway","channel","chart","invention","campground","typewriter","notorious","grandstand","inlet","silver","confiscate","chord","fugue","personnel","subito","interview","cannon","marketing","knob","mountains","pizzicato","sale","coastline","video","seashore","pass","oasis","receipt","andante","outgoing","trade","career","ship","marauder","fortissimo","capture","turnpike","geology","depression","crest","battle","rock","firth","carnival","slough","chest","beat","swagger","fight","unpack","drive","scherzo","crescent","hook","vivace","vent","grandioso","kidnap","notes","bar","train","customs","hotel","tie","gangplank","loot","benefits","equipment","feat","port","photos","coast","vile","diminuendo","supertonic","buccaneer","bags","banner","walk","paperweight","freeway","notation","falls","shareholder","boss","deck","mesa","evil","monkey","arête","promotion","largo","rope","product","loss","pictures","camera","volcano","play","foreigner","negotiation","roundabout","pegleg","principal","bookkeeping","vibrato","visa","underpass","consumer","sforzando","neck","commercial","range","grotto","landing","revolt","cage","pirate","union","kill","daring","presto","gun","overdraft","gear","highway","stream","resign","board","deal","span","employ","cab","sign","headhunter","wholesale","marcato","cubicle","ferocious","island","thief","phrase","steal","rat","passport","semplice","islet","arioso","outcasts","parade","unemployment","fourth","headquarters","trapeze","debit","jetsam","bight","prospects","geomorphology","station","department","roadway","export","subway","stem","depart","beach","viaduct","meter","shipmate","admission","continent","crevasse","canon","treasurer","airfare","rivulet","street","boulevard","sword","rubato","fairground","service","manager","challenge","suitcase","fork","income","gymnast","calliope","entrepreneur","capitalist","minim","contraband","loan","seabed","purchasing","forzando","cutthroat","hurricane","cadence","fjord","shipping","booth","rapids","offline","gunner","libretto","fund","route","yardang","postcard","glen","rhythm","plank","ruthless","money","photographs","revenge","taxi","mountain","compromise","lead","baggage","prowl","strait","circle","expedition","bow","bar","borrow","lagoon","seal","trainer","deadline","canal","staccato","guide","itinerary","recreation","corporate","automobile","limey","graph","insurance","moderato","hill","barbaric","trunk","clef","overhead","coupon","offer","binoculars","hemidemisemiquaver","map"]

positive :: [Text]
positive = ["attractive","proud","glamorous","poised","lively","choice","engaging","phenomenal","powerful","brilliant","champ","familiar","dazzling","action","nutritious","unwavering","congratulation","free","sparkling","meritorious","positive","agreeable","approve","popular","divine","successful","worthy","accepted","plentiful","refined","jubilant","celebrated","imaginative","skillful","honest","admire","innovate","excellent","thorough","imagine","legendary","transforming","instinctive","reassuring","elegant","harmonious","clean","principled","satisfactory","ecstatic","genius","stunning","stirring","champion","electrifying","kind","vigorous","keen","merit","inventive","thrilling","cool","charming","fair","prominent","ethical","wealthy","whole","honored","luminous","heavenly","right","happy","innovative","accomplishment","up","exquisite","handsome","fantastic","reliable","vivacious","angelic","virtuous","fitting","fabulous","learned","robust","trusting","acclaimed","pleasurable","masterful","willing","productive","victory","open","affirmative","delight","believe","ideal","energetic","flourishing","effective","special","resounding","jovial","well","distinguished","vital","brave","prepared","effervescent","refreshing","miraculous","commend","courageous","healing","ready","pleasant","giving","favorable","intellectual","esteemed","generous","agree","novel","reward","hearty","sunny","honorable","spirited","awesome","welcome","zealous","funny","fortunate","exciting","valued","accomplish","intelligent","composed","thriving","endorsed","enchanting","victorious","effortless","superb","instantaneous","unreal","rejoice","amazing","marvelous","upright","restored","tops","delightful","success","adorable","aptitude","enthusiastic","famous","motivating","rewarding","bubbly","remarkable","moving","active","good","classic","gorgeous","lucid","meaningful","constant","healthy","bountiful","graceful","achievement","efficient","super","natural","wonderful","grin","lovely","laugh","zeal","terrific","beneficial","knowing","nurturing","safe","great","polished","yummy","wondrous","cute","supporting","adventure","tranquil","fine","respected","calm","skilled","beautiful","encouraging","idea","protected","perfect","vibrant","glowing","wholesome","stupendous","quiet","secure","certain","beaming","nice","appealing","bounty","transformative","intuitive","instant","fetching","progress","quality","growing","quick","earnest","pretty","energized","lucky","green","easy","okay","absolutely","upstanding","optimistic","classical","seemly","smile","truthful","independent","essential","fresh","light","simple","impressive","bravo","soulful","affluent","genuine","bliss","knowledgeable","surprising","upbeat","friendly","creative","spiritual","cheery","paradise"]

negative :: [Text]
negative = ["hate","barbed","yucky","poor","confused","lousy","unwise","alarming","junky","frighten","despicable","undermine","wary","broken","petty","reject","scream","hideous","unhappy","quirky","sinister","shocking","impossible","severe","noxious","fail","wound","nasty","damage","hard","sticky","woeful","ignorant","repulsive","slimy","horrendous","substandard","damaging","dead","dismal","gawky","unjust","worthless","insipid","smelly","naive","odious","corrupt","scare","hurt","pessimistic","fight","vile","appalling","revolting","stinky","lumpy","mean","stupid","apathy","unwholesome","savage","quit","objectionable","imperfect","frightful","depressed","stuck","coarse","zero","renege","shoddy","anxious","offensive","negate","grimace","prejudice","disgusting","messy","unpleasant","hurtful","creepy","negative","repugnant","jealous","suspicious","injurious","yell","enraged","no","unwieldy","insane","feeble","inane","unwelcome","beneath","scary","stressful","grim","revenge","rude","grotesque","ghastly","rotten","corrosive","spiteful","deny","wicked","homely","adverse","deplorable","villainous","deformed","sobbing","immature","deprived","rocky","abysmal","banal","lose","naughty","cutting","annoy","pain","never","disease","greed","plain","unfavorable","vindictive","haggard","nondescript","contrary","clumsy","guilty","nonsense","distress","unsatisfactory","insidious","oppressive","menacing","misshapen","cruel","evil","malicious","terrifying","threatening","crazy","can't","disheveled","dreadful","grave","sick","unfair","stormy","vicious","ruthless","belligerent","detrimental","collapse","upset","dirty","boring","faulty","nobody","untoward","questionable","unwanted","dreary","infernal","vice","unsightly","missing","harmful","perturb","moldy","suspect","ignore","awful","misunderstood","monstrous","dastardly","reptilian","repellant","callous","inelegant","horrible","icky","sorry","criminal","gross","poisonous","angry","terrible","injure","foul","cold","sickening","filthy","dishonorable","eroding","moan","unhealthy","dishonest","unlucky","fear","gruesome","hostile","ugly","upset","decaying","contradictory","bemoan","atrocious","tense","weary","cry"]

places :: [Text]
places = ["massachusetts","washington","maryland","oregon","kansas","oklahoma","alaska","wyoming","hawaii","idaho","colorado","alabama","ohio","illinois","virginia","maine","pennsylvania","kentucky","michigan","montana","wisconsin","indiana","louisiana","vermont","arkansas","missouri","iowa","tennessee","connecticut","arizona","california","nevada","nebraska","minnesota","mississippi","delaware","florida","georgia","utah","texas","moldova","burma","mauritania","lithuania","vanuatu","malawi","samoa","senegal","ecuador","austria","eritrea","honduras","benin","jamaica","palau","bulgaria","afghanistan","singapore","mali","georgia","argentina","estonia","thailand","turkmenistan","botswana","armenia","spain","andorra","madagascar","vietnam","burundi","belgium","azerbaijan","guatemala","comoros","haiti","netherlands","ukraine","lebanon","ethiopia","tunisia","croatia","algeria","chile","bolivia","sudan","romania","iraq","iran","canada","israel","venezuela","belarus","micronesia","australia","syria","peru","libya","guinea","switzerland","monaco","niger","uzbekistan","mauritius","panama","nepal","swaziland","malaysia","philippines","macedonia","portugal","germany","tuvalu","guinea-bissau","france","qatar","kiribati","zambia","nicaragua","liechtenstein","gabon","tajikistan","kyrgyzstan","brazil","suriname","mongolia","dominica","norway","chad","denmark","fiji","zimbabwe","cambodia","russia","slovakia","uganda","barbados","luxembourg","cuba","tonga","bangladesh","ghana","nigeria","poland","yemen","malta","seychelles","latvia","djibouti","lesotho","mexico","turkey","japan","iceland","guyana","nauru","morocco","albania","egypt","bahrain","india","pakistan","indonesia","grenada","cameroon","italy","angola","jordan","greece","liberia","maldives","finland","oman","slovenia","cyprus","uruguay","paraguay","namibia","tanzania","china","sweden","kenya","belize","hungary","ireland","serbia","rwanda","bahamas","kazakhstan","cambodia","togo","brunei","laos","somalia","montenegro","bhutan","mozambique","kuwait","stadium","school","undertaker","museum","market","shop","milliner","theater","motel","boutique","park","tailor","cafe","drugstore","lumberyard","pharmacy","auditorium","gymnasium","brasserie","supermarket","pawnshop","airport","library","stationer","university","arcade","emporium","cinema","automat","bookstore","bank","optometrist","dressmaker","aquarium","lighthouse","playground","tearoom","arena","florist","tavern","courthouse","diner","mall","college","store","restaurant","saloon","hairdresser","bakery","bistro","cafeteria","salon","retailer","nursery","barbershop","clinic","gallery","delicatessen","deli","hotel","haberdashery"]
