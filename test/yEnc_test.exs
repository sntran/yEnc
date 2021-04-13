defmodule YEncTest do
  use YEnc.TestCase, async: true

  doctest YEnc, tags: []

  setup do
    file = "fixtures/binaries/1k.bin"
    line_or_bytes = 64
    stream = File.stream!(file, [], line_or_bytes)

    [stream: stream, line_or_bytes: line_or_bytes]
  end

  describe "encode/1" do
    test "byte" do
      assert YEnc.encode(<<>>) === <<>>
      assert YEnc.encode(<<0>>) === <<42>>
      assert YEnc.encode(<<1>>) === <<43>>
      assert YEnc.encode(<<2>>) === <<44>>
      assert YEnc.encode(<<3>>) === <<45>>
      # @TODO: Maybe we need to escape this too?
      assert YEnc.encode(<<4>>) === <<46>>
      assert YEnc.encode(<<5>>) === <<47>>
      assert YEnc.encode(<<6>>) === <<48>>
      assert YEnc.encode(<<7>>) === <<49>>
      assert YEnc.encode(<<8>>) === <<50>>
      assert YEnc.encode(<<9>>) === <<51>>
      assert YEnc.encode(<<10>>) === <<52>>
      assert YEnc.encode(<<11>>) === <<53>>
      assert YEnc.encode(<<12>>) === <<54>>
      assert YEnc.encode(<<13>>) === <<55>>
      assert YEnc.encode(<<14>>) === <<56>>
      assert YEnc.encode(<<15>>) === <<57>>
      assert YEnc.encode(<<16>>) === <<58>>
      assert YEnc.encode(<<17>>) === <<59>>
      assert YEnc.encode(<<18>>) === <<60>>
      # `=`
      assert YEnc.encode(<<19>>) === <<61, 125>>
      assert YEnc.encode(<<20>>) === <<62>>
      assert YEnc.encode(<<21>>) === <<63>>
      assert YEnc.encode(<<22>>) === <<64>>
      assert YEnc.encode(<<23>>) === <<65>>
      assert YEnc.encode(<<24>>) === <<66>>
      assert YEnc.encode(<<25>>) === <<67>>
      assert YEnc.encode(<<26>>) === <<68>>
      assert YEnc.encode(<<27>>) === <<69>>
      assert YEnc.encode(<<28>>) === <<70>>
      assert YEnc.encode(<<29>>) === <<71>>
      assert YEnc.encode(<<30>>) === <<72>>
      assert YEnc.encode(<<31>>) === <<73>>
      assert YEnc.encode(<<32>>) === <<74>>
      assert YEnc.encode(<<33>>) === <<75>>
      assert YEnc.encode(<<34>>) === <<76>>
      assert YEnc.encode(<<35>>) === <<77>>
      assert YEnc.encode(<<36>>) === <<78>>
      assert YEnc.encode(<<37>>) === <<79>>
      assert YEnc.encode(<<38>>) === <<80>>
      assert YEnc.encode(<<39>>) === <<81>>
      assert YEnc.encode(<<40>>) === <<82>>
      assert YEnc.encode(<<41>>) === <<83>>
      assert YEnc.encode(<<42>>) === <<84>>
      assert YEnc.encode(<<43>>) === <<85>>
      assert YEnc.encode(<<44>>) === <<86>>
      assert YEnc.encode(<<45>>) === <<87>>
      assert YEnc.encode(<<46>>) === <<88>>
      assert YEnc.encode(<<47>>) === <<89>>
      assert YEnc.encode(<<48>>) === <<90>>
      assert YEnc.encode(<<49>>) === <<91>>
      assert YEnc.encode(<<50>>) === <<92>>
      assert YEnc.encode(<<51>>) === <<93>>
      assert YEnc.encode(<<52>>) === <<94>>
      assert YEnc.encode(<<53>>) === <<95>>
      assert YEnc.encode(<<54>>) === <<96>>
      assert YEnc.encode(<<55>>) === <<97>>
      assert YEnc.encode(<<56>>) === <<98>>
      assert YEnc.encode(<<57>>) === <<99>>
      assert YEnc.encode(<<58>>) === <<100>>
      assert YEnc.encode(<<59>>) === <<101>>
      assert YEnc.encode(<<60>>) === <<102>>
      assert YEnc.encode(<<61>>) === <<103>>
      assert YEnc.encode(<<62>>) === <<104>>
      assert YEnc.encode(<<63>>) === <<105>>
      assert YEnc.encode(<<64>>) === <<106>>
      assert YEnc.encode(<<65>>) === <<107>>
      assert YEnc.encode(<<66>>) === <<108>>
      assert YEnc.encode(<<67>>) === <<109>>
      assert YEnc.encode(<<68>>) === <<110>>
      assert YEnc.encode(<<69>>) === <<111>>
      assert YEnc.encode(<<70>>) === <<112>>
      assert YEnc.encode(<<71>>) === <<113>>
      assert YEnc.encode(<<72>>) === <<114>>
      assert YEnc.encode(<<73>>) === <<115>>
      assert YEnc.encode(<<74>>) === <<116>>
      assert YEnc.encode(<<75>>) === <<117>>
      assert YEnc.encode(<<76>>) === <<118>>
      assert YEnc.encode(<<77>>) === <<119>>
      assert YEnc.encode(<<78>>) === <<120>>
      assert YEnc.encode(<<79>>) === <<121>>
      assert YEnc.encode(<<80>>) === <<122>>
      assert YEnc.encode(<<81>>) === <<123>>
      assert YEnc.encode(<<82>>) === <<124>>
      assert YEnc.encode(<<83>>) === <<125>>
      assert YEnc.encode(<<84>>) === <<126>>
      assert YEnc.encode(<<85>>) === <<127>>
      assert YEnc.encode(<<86>>) === <<128>>
      assert YEnc.encode(<<87>>) === <<129>>
      assert YEnc.encode(<<88>>) === <<130>>
      assert YEnc.encode(<<89>>) === <<131>>
      assert YEnc.encode(<<90>>) === <<132>>
      assert YEnc.encode(<<91>>) === <<133>>
      assert YEnc.encode(<<92>>) === <<134>>
      assert YEnc.encode(<<93>>) === <<135>>
      assert YEnc.encode(<<94>>) === <<136>>
      assert YEnc.encode(<<95>>) === <<137>>
      assert YEnc.encode(<<96>>) === <<138>>
      assert YEnc.encode(<<97>>) === <<139>>
      assert YEnc.encode(<<98>>) === <<140>>
      assert YEnc.encode(<<99>>) === <<141>>
      assert YEnc.encode(<<100>>) === <<142>>
      assert YEnc.encode(<<101>>) === <<143>>
      assert YEnc.encode(<<102>>) === <<144>>
      assert YEnc.encode(<<103>>) === <<145>>
      assert YEnc.encode(<<104>>) === <<146>>
      assert YEnc.encode(<<105>>) === <<147>>
      assert YEnc.encode(<<106>>) === <<148>>
      assert YEnc.encode(<<107>>) === <<149>>
      assert YEnc.encode(<<108>>) === <<150>>
      assert YEnc.encode(<<109>>) === <<151>>
      assert YEnc.encode(<<110>>) === <<152>>
      assert YEnc.encode(<<111>>) === <<153>>
      assert YEnc.encode(<<112>>) === <<154>>
      assert YEnc.encode(<<113>>) === <<155>>
      assert YEnc.encode(<<114>>) === <<156>>
      assert YEnc.encode(<<115>>) === <<157>>
      assert YEnc.encode(<<116>>) === <<158>>
      assert YEnc.encode(<<117>>) === <<159>>
      assert YEnc.encode(<<118>>) === <<160>>
      assert YEnc.encode(<<119>>) === <<161>>
      assert YEnc.encode(<<120>>) === <<162>>
      assert YEnc.encode(<<121>>) === <<163>>
      assert YEnc.encode(<<122>>) === <<164>>
      assert YEnc.encode(<<123>>) === <<165>>
      assert YEnc.encode(<<124>>) === <<166>>
      assert YEnc.encode(<<125>>) === <<167>>
      assert YEnc.encode(<<126>>) === <<168>>
      assert YEnc.encode(<<127>>) === <<169>>
      assert YEnc.encode(<<128>>) === <<170>>
      assert YEnc.encode(<<129>>) === <<171>>
      assert YEnc.encode(<<130>>) === <<172>>
      assert YEnc.encode(<<131>>) === <<173>>
      assert YEnc.encode(<<132>>) === <<174>>
      assert YEnc.encode(<<133>>) === <<175>>
      assert YEnc.encode(<<134>>) === <<176>>
      assert YEnc.encode(<<135>>) === <<177>>
      assert YEnc.encode(<<136>>) === <<178>>
      assert YEnc.encode(<<137>>) === <<179>>
      assert YEnc.encode(<<138>>) === <<180>>
      assert YEnc.encode(<<139>>) === <<181>>
      assert YEnc.encode(<<140>>) === <<182>>
      assert YEnc.encode(<<141>>) === <<183>>
      assert YEnc.encode(<<142>>) === <<184>>
      assert YEnc.encode(<<143>>) === <<185>>
      assert YEnc.encode(<<144>>) === <<186>>
      assert YEnc.encode(<<145>>) === <<187>>
      assert YEnc.encode(<<146>>) === <<188>>
      assert YEnc.encode(<<147>>) === <<189>>
      assert YEnc.encode(<<148>>) === <<190>>
      assert YEnc.encode(<<149>>) === <<191>>
      assert YEnc.encode(<<150>>) === <<192>>
      assert YEnc.encode(<<151>>) === <<193>>
      assert YEnc.encode(<<152>>) === <<194>>
      assert YEnc.encode(<<153>>) === <<195>>
      assert YEnc.encode(<<154>>) === <<196>>
      assert YEnc.encode(<<155>>) === <<197>>
      assert YEnc.encode(<<156>>) === <<198>>
      assert YEnc.encode(<<157>>) === <<199>>
      assert YEnc.encode(<<158>>) === <<200>>
      assert YEnc.encode(<<159>>) === <<201>>
      assert YEnc.encode(<<160>>) === <<202>>
      assert YEnc.encode(<<161>>) === <<203>>
      assert YEnc.encode(<<162>>) === <<204>>
      assert YEnc.encode(<<163>>) === <<205>>
      assert YEnc.encode(<<164>>) === <<206>>
      assert YEnc.encode(<<165>>) === <<207>>
      assert YEnc.encode(<<166>>) === <<208>>
      assert YEnc.encode(<<167>>) === <<209>>
      assert YEnc.encode(<<168>>) === <<210>>
      assert YEnc.encode(<<169>>) === <<211>>
      assert YEnc.encode(<<170>>) === <<212>>
      assert YEnc.encode(<<171>>) === <<213>>
      assert YEnc.encode(<<172>>) === <<214>>
      assert YEnc.encode(<<173>>) === <<215>>
      assert YEnc.encode(<<174>>) === <<216>>
      assert YEnc.encode(<<175>>) === <<217>>
      assert YEnc.encode(<<176>>) === <<218>>
      assert YEnc.encode(<<177>>) === <<219>>
      assert YEnc.encode(<<178>>) === <<220>>
      assert YEnc.encode(<<179>>) === <<221>>
      assert YEnc.encode(<<180>>) === <<222>>
      assert YEnc.encode(<<181>>) === <<223>>
      assert YEnc.encode(<<182>>) === <<224>>
      assert YEnc.encode(<<183>>) === <<225>>
      assert YEnc.encode(<<184>>) === <<226>>
      assert YEnc.encode(<<185>>) === <<227>>
      assert YEnc.encode(<<186>>) === <<228>>
      assert YEnc.encode(<<187>>) === <<229>>
      assert YEnc.encode(<<188>>) === <<230>>
      assert YEnc.encode(<<189>>) === <<231>>
      assert YEnc.encode(<<190>>) === <<232>>
      assert YEnc.encode(<<191>>) === <<233>>
      assert YEnc.encode(<<192>>) === <<234>>
      assert YEnc.encode(<<193>>) === <<235>>
      assert YEnc.encode(<<194>>) === <<236>>
      assert YEnc.encode(<<195>>) === <<237>>
      assert YEnc.encode(<<196>>) === <<238>>
      assert YEnc.encode(<<197>>) === <<239>>
      assert YEnc.encode(<<198>>) === <<240>>
      assert YEnc.encode(<<199>>) === <<241>>
      assert YEnc.encode(<<200>>) === <<242>>
      assert YEnc.encode(<<201>>) === <<243>>
      assert YEnc.encode(<<202>>) === <<244>>
      assert YEnc.encode(<<203>>) === <<245>>
      assert YEnc.encode(<<204>>) === <<246>>
      assert YEnc.encode(<<205>>) === <<247>>
      assert YEnc.encode(<<206>>) === <<248>>
      assert YEnc.encode(<<207>>) === <<249>>
      assert YEnc.encode(<<208>>) === <<250>>
      assert YEnc.encode(<<209>>) === <<251>>
      assert YEnc.encode(<<210>>) === <<252>>
      assert YEnc.encode(<<211>>) === <<253>>
      assert YEnc.encode(<<212>>) === <<254>>
      assert YEnc.encode(<<213>>) === <<255>>
      # `NULL`
      assert YEnc.encode(<<214>>) === <<61, 64>>
      assert YEnc.encode(<<215>>) === <<1>>
      assert YEnc.encode(<<216>>) === <<2>>
      assert YEnc.encode(<<217>>) === <<3>>
      assert YEnc.encode(<<218>>) === <<4>>
      assert YEnc.encode(<<219>>) === <<5>>
      assert YEnc.encode(<<220>>) === <<6>>
      assert YEnc.encode(<<221>>) === <<7>>
      assert YEnc.encode(<<222>>) === <<8>>
      # `TAB`
      assert YEnc.encode(<<223>>) === <<61, 73>>
      # `LF`
      assert YEnc.encode(<<224>>) === <<61, 74>>
      assert YEnc.encode(<<225>>) === <<11>>
      assert YEnc.encode(<<226>>) === <<12>>
      # `CR`
      assert YEnc.encode(<<227>>) === <<61, 77>>
      assert YEnc.encode(<<228>>) === <<14>>
      assert YEnc.encode(<<229>>) === <<15>>
      assert YEnc.encode(<<230>>) === <<16>>
      assert YEnc.encode(<<231>>) === <<17>>
      assert YEnc.encode(<<232>>) === <<18>>
      assert YEnc.encode(<<233>>) === <<19>>
      assert YEnc.encode(<<234>>) === <<20>>
      assert YEnc.encode(<<235>>) === <<21>>
      assert YEnc.encode(<<236>>) === <<22>>
      assert YEnc.encode(<<237>>) === <<23>>
      assert YEnc.encode(<<238>>) === <<24>>
      assert YEnc.encode(<<239>>) === <<25>>
      assert YEnc.encode(<<240>>) === <<26>>
      assert YEnc.encode(<<241>>) === <<27>>
      assert YEnc.encode(<<242>>) === <<28>>
      assert YEnc.encode(<<243>>) === <<29>>
      assert YEnc.encode(<<244>>) === <<30>>
      assert YEnc.encode(<<245>>) === <<31>>
      # `SPACE`
      assert YEnc.encode(<<246>>) === <<61, 96>>
      assert YEnc.encode(<<247>>) === <<33>>
      assert YEnc.encode(<<248>>) === <<34>>
      assert YEnc.encode(<<249>>) === <<35>>
      assert YEnc.encode(<<250>>) === <<36>>
      assert YEnc.encode(<<251>>) === <<37>>
      assert YEnc.encode(<<252>>) === <<38>>
      assert YEnc.encode(<<253>>) === <<39>>
      assert YEnc.encode(<<254>>) === <<40>>
      assert YEnc.encode(<<255>>) === <<41>>
    end
  end
end
