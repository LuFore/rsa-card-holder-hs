import Graphics.Implicit
import Data.Map (Map, map, (!), insert, fromList)
import Control.Lens
import Linear.V3

--------------------------------------------------------------------------------
rsa_token_big = fromList
	[ ("bulb radius", (28 / 2) )
	, ("length"     , 79 )
	, ("height"     , 9.3  )
	, ("width"      , 21 )
	, ("dangler"    , 5)
	]
	
rsa_token = fromList
	[ ("bulb radius", (28 / 2) )
	, ("length"     , 59 )
	, ("height"     , 9.3  )
	, ("width"      , 21 )
	, ("dangler"    , 5)
	]
	
card = V3 87.0 55.0 1.2

wall = 2 

tab_space = V2 20 $ wall + 7

--------------------------------------------------------------------------------

degrees :: ℝ -> ℝ
degrees rad = rad/360 * 2 * 3.1

axis :: ℝ3 -> Char -> ℝ
axis v3 axe =
	case axe of
		'x' -> (v3 ^. _x)
		'y' -> (v3 ^. _y)
		'z' -> (v3 ^. _z)

boundedRound :: ℝ -> SymbolicObj3 -> SymbolicObj3
boundedRound round og_obj = intersect [ withRounding round og_obj, og_obj ]

mk_token :: (Map [Char] ℝ) -> SymbolicObj3
mk_token rsa
	= let
	bulb_rad = rsa ! "bulb radius"
	width    = rsa ! "width"
	height   = rsa ! "height"
	length   = rsa ! "length"
	dangler  = rsa ! "dangler"
	body     = cube False $ V3 (length - bulb_rad) width height
	in 
	translate (V3 (bulb_rad + dangler) bulb_rad 0 )
	$ union 
		[ translate (V3 0 (- width / 2) 0) body
		, cylinder bulb_rad height
		, translate ( V3 (- bulb_rad - dangler) 0 ( height / 2 ))
				$ rotate3 ( V3 0 (degrees 90) 0 ) (cylinder dangler (dangler * 2) )
		]

mk_card :: ℝ3 -> SymbolicObj3
mk_card card = cube False card

remove_dangler ::(Map [Char] ℝ) -> (Map [Char] ℝ)
remove_dangler rsa 
	= insert "dangler" 0 rsa

fat_rsa :: ℝ -> (Map [Char] ℝ) ->(Map [Char] ℝ)
fat_rsa wall rsa  
	= insert "bulb radius" 
		(( rsa ! "bulb radius" ) + (wall / 2) )
		$	Data.Map.map (\x -> x + wall) rsa

rsa_cutout :: (Map [Char] ℝ) ->  ℝ -> SymbolicObj3
rsa_cutout rsa wall 
	= let
	big_rsa = mk_token rsa
	lil_rsa = mk_token $ remove_dangler (insert "height" wall (fat_rsa (- wall * 2) rsa))
	lil_rsa_mov =  V3 ( (rsa!"dangler") + wall ) ( wall ) ( rsa ! "height" )
	in 
	union [ big_rsa , translate lil_rsa_mov lil_rsa ] 

card_cutout :: ℝ3 -> ℝ2 -> ℝ -> SymbolicObj3
card_cutout card tab wall  
	= let
	ncard = V3 (axis card 'x') (axis card 'y') ((axis card 'z') + wall )
	basic_tab = mk_tab (V3 (tab ^. _x) (tab ^. _y) wall ) wall 
	x_trans =  ( (axis card 'x') - (tab ^. _x) ) / 2 
	b_tab = 
		translate 
			(V3 x_trans  0 0 )
			basic_tab
	u_tab = 
		translate 
		( V3 x_trans (axis card 'y') wall ) 
		$ rotate3
			( V3 (degrees 185) 0 0 )
			basic_tab
	in 
	differenceR ( wall / 2 ) (mk_card ncard)
	[ b_tab
	, u_tab
	]

mk_tab :: ℝ3 -> ℝ -> SymbolicObj3 
mk_tab size wall =
	differenceR (wall/2)
		(boundedRound (wall/2) (cube False size))
		[ translate (V3 wall 0 0) 
			$ boundedRound (wall/2)( cube False (V3 ((axis size 'x') - (wall * 2)) ((axis size 'y') - wall) (axis size 'z'))
		)]

calc_nrsa_mov :: (Map [Char] ℝ) -> ℝ3 ->  ℝ -> ℝ3
calc_nrsa_mov rsa card wall 
	= let 
	-- nrsa = remove_dangler ( fat_rsa (wall * 2) rsa ) 
	ncard = card + ( pure ( wall * 2 ) )
	card_x = axis ncard 'x'
	card_y = axis ncard 'y'
	in 
	V3 
		( ( ( card_x - ( rsa !"length" ) ) / 2 ) - (rsa ! "dangler") ) -- - wall )
		( (card_y / 2)  - (rsa ! "bulb radius") ) -- + wall ) 
		( axis card 'z' + wall )


workpiece :: (Map [Char] ℝ) -> ℝ3 -> ℝ2 -> ℝ -> SymbolicObj3
workpiece rsa card tab wall 
	= let 
	nrsa = insert "height" (rsa!"height") ( remove_dangler ( fat_rsa (wall * 2) rsa ))
	ncard = card + ( pure ( wall * 2 ) )
	tabby = mk_tab (V3 (tab ^. _x) (tab ^. _y) (axis ncard 'z') ) wall 
	card_x = axis ncard 'x'
	card_y = axis ncard 'y'
	rsa_mov = V3 
		( ( card_x - ( nrsa !"length" ) ) / 2 )
		( (card_y / 2)  - (nrsa ! "bulb radius") ) 
		( axis ncard 'z' )
	tab_mov = V3
		( ( card_x  - (tab ^. _x) ) / 2 ) 
		( card_y )
		0 --( ( axis ncard 'z') / 2 - (wall / 2)) 
	in
	unionR wall 
		[ translate tab_mov tabby
		, unionR ( wall * 2 ) 
			[ translate rsa_mov (mk_token nrsa) 
			, boundedRound wall $ mk_card ncard  
			]
		]

final :: (Map [Char] ℝ) -> ℝ3 -> ℝ2 -> ℝ -> SymbolicObj3
final rsa card tab wall 
	=
	let 
	rsa_mov = calc_nrsa_mov rsa card wall
	in
	differenceR (wall/2)
		( workpiece rsa card tab wall )
		[ translate (V3 wall wall 0) $ card_cutout card (V2 (tab ^. _x) (wall * 2)) wall
		, translate rsa_mov $ rsa_cutout rsa wall
		]

testies:: ℝ3 -> ℝ2 -> ℝ -> SymbolicObj3
testies card tab wall  
	= let
	ncard = V3 (axis card 'x') (axis card 'y') ((axis card 'z') + wall )
	basic_tab = mk_tab (V3 (tab ^. _x) (tab ^. _y) wall ) wall 
	x_trans =  ( (axis card 'x') - (tab ^. _x) ) / 2 
	b_tab = 
		translate 
			(V3 x_trans  0 0 )
			basic_tab
	u_tab = 
		translate 
		( V3 x_trans (wall + (axis card 'y')) wall ) 
		$ rotate3
			( V3 (degrees 180) 0 0 )
			basic_tab
	in 
	union 
	[ b_tab
	, u_tab
	]

out = final rsa_token card tab_space wall
main = writeSTL 0.5 "small.stl" out 
