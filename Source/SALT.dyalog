:Namespace SALT
    (⎕IO ⎕ML ⎕WX)←1 0 3

    ∇ Boot
     ⍝ Move a local copy of SALT into ⎕SE
      ⎕SE.⎕FIX¨#.SALT.(⎕SRC¨SALTUtils SALT Parser UnicodeFile)
     
     ⍝ The Dyalog namespace is a bit more complicated
      ⎕EX'⎕se.Dyalog' ⋄ '⎕SE.Dyalog'⎕NS ⍬
     
     ⍝ We cannot do
     ⍝   'Dyalog'⎕SE.⎕NS ⎕OR'SALT.Dyalog'
     ⍝ because this will keep a reference to # in ⎕SE
     ⍝ so we do this:
      ⎕SE.Dyalog.⎕FIX ⎕SRC Dyalog.Utils
      '⎕se.Dyalog.Callbacks'⎕NS ⎕OR'Dyalog.Callbacks'
    ∇

    :Namespace Dyalog
        (⎕IO ⎕ML ⎕WX)←1 0 3

        :Namespace Callbacks
⍝ === VARIABLES ===

            ConfirmEdit←1


⍝ === End of variables definition ===

            (⎕IO ⎕ML ⎕WX)←1 0 1

            ∇ WSLoaded msg;tb;univ;f;dev;forced;dld;loc;tn;b;code;su;t
     ⍝ Handle WS )LOAD event
              :If forced←msg≡1
              :OrIf 'WorkspaceLoaded'≡2⊃msg
                  :If 0≠⎕NC f←'⎕SE.cbtop.bandtb5.tb' ⍝ skip non GUI environments
                      tb←⍎f ⍝ Initialize font picker
                      univ←80∊⎕DR''
                      tb.font.Items←{⍵[⍋↑⍵]}univ{(⍺∨1=3⊃¨⍵)/1⊃¨⍵}'.'⎕WG'FontList'
                      tb.(font.Text size.Value)←2↑⎕SE.FontObj
                  :EndIf
             
         ⍝ To determine if the session just started we use ⎕AI
         ⍝ Keyboard time will be 0 the 1st time it is loaded but this will
         ⍝ be wrong in runtimes so we'll use the elapsed time there, assuming
         ⍝ that 15 seconds is enough to start APL.
         ⍝ In the worst case, if another ws is )LOADed within 15 sec of startup
         ⍝ SALT will be reloaded.
                  t←⎕AI
                  dev←1∊'Dev'⍷4⊃'.'⎕WG'aplversion'
                  :If forced∨⎕AI[3+dev]≤15000×~dev ⍝ dev: kb time=0, runtime: elapsed ≤ 15 sec
         ⍝ This code used to be divided into subfns but for sanity sake it is all together now.
         ⍝ 1st find the location of SALTUtils:
             ⍝ ⎕PROFILE'start'
                      {
                          dld←{(-'/\'∊⍨¯1↑⍵)↓⍵} ⍝ Drop Last Deliminiter fn
             
             ⍝ The SALT location may be supplied on the command line
                          loc←{0∊⍴loc←dld 2 ⎕NQ'.' 'GetEnvironment' 'SALT':{
             ⍝ If it isn't it should be found here:
                                  f←dld 2 ⎕NQ'.' 'GetEnvironment' 'DYALOG'
                                  (dld(¯3×'bin'≡¯3↑f)↓f),'/SALT'}⍵ ⋄ loc}⍬
                          loc,←'/core/SALTUtils.dyalog'
             
             ⍝ Things should not go wrong but if they do we don't want to leave a pending stack
                          code←{⍝0::⎕←'SALT initialization failed: ',⎕DMX.Message ⋄
             ⍝ Read the file and translate into Unicode after removing CRs
                              code←256|⎕NREAD tn 83,2↑⎕NSIZE tn←⍵ ⎕NTIE 0 ⋄ x←⎕NUNTIE tn
                              239 187 191≡3↑code:'UTF-8'⎕UCS 3↓code~13 ⍝ UTF-8 header
                              ∨/b←(2 2⍴254 255 255)∧.=2↑code:⎕UCS 1↓13~⍨(256*b)+.×⍨(2,⍨(⍴code)÷2)⍴code ⍝ UCS2?
                 ⍝ Assume UTF-8 and trap any error
                              'UTF-8'⎕UCS code~13}loc
             
             ⍝ Define the (SALTUtils) namespace locally
                          su←0 ⎕FIX{1↓¨{(⍵=⎕IO⊃⍵)⊂⍵}⍵,⍨⎕UCS 10}code
                          su.BootSALT ⍝ finally, call the bootstrap fn
                      }⍬
             ⍝ ⎕PROFILE'stop'
                  :EndIf
             
     ⍝     ⎕AI-t
              :Else
             
                  :If 'Spin'≡2⊃msg
                      ⎕NQ(1⊃msg)'Change'
                  :Else
                      :If 'KeyPress'≢2⊃msg
                      :OrIf 'ER'≡3⊃msg
                          tb←(⍎⊃msg).##
                          '⎕SE'⎕WS'Font'(f←(tb.font.Text)(⌊tb.size.Value)1 0 0 400)
                  ⍝ Also update status bar font
                          f[2]←¯8+1⊃'⎕SE.cbbot.bandsb2.sb'⎕WG'Size'
                          '⎕se.cbbot.bandsb2.sb.curobj'⎕WS'Font'f
                      :EndIf
                  :EndIf
              :EndIf
            ∇

        :EndNamespace
        :Namespace Utils
⍝ === VARIABLES ===

            Version←1.05

            lc←'abcdefghijklmnopqrstuvwxyzàáâãåèéêëòóôõöøùúûäæüìíîïðçñ'

            uc←'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÅÈÉÊËÒÓÔÕÖØÙÚÛÄÆÜÌÍÎÏÐÇÑ'


⍝ === End of variables definition ===

            (⎕IO ⎕ML ⎕WX)←1 1 3

            cut←{⍺←⍵∊1↑⍵ ⋄ ⎕ML←0 ⋄ 1↓¨⍺⊂⍵}

              disp←{⎕IO ⎕ML←0                             ⍝ Boxed sketch of nested array.
             
                  ⍺←⍬ ⋄ opts←⍺,(⍴,⍺)↓1 1 1 0 ⎕PP          ⍝ option defaults:
                  dec bch ctd sep ⎕PP←5↑opts              ⍝ decor, smooth, centred, separate pp.
             
                  ul uc ur←bch⊃⌽'┌┬┐' '.'                 ⍝ upper──┐ ┌───left
                  ml mc mr←bch⊃⌽'├┼┤' '|+|'               ⍝ middle─┼×┼─centre
                  ll lc lr←bch⊃⌽'└┴┘' ''''                ⍝ lower──┘ └──right
                  vt hz←bch⊃⌽'│─' '|-'                    ⍝ vertical and horizontal.
             
                  box←{                                   ⍝ Recursive boxing of nested array.
                      isor ⍵:⎕FMT⊂⍵                       ⍝ ⎕or: '∇name'.
                      1=≡,⍵:dec open ⎕FMT dec open ⍵      ⍝ simple array: format.
                      mat←matr dec open ⍵                 ⍝ matrix of opened subarrays.
                      dec<0∊⍴mat:0/∇ 1 open mat           ⍝ undecorated null: empty result.
                      subs←aligned ∇¨mat                  ⍝ aligned boxed subarrays. [10999]
                      (⊃⍴⍴⍵)gaps ⍵ plane subs             ⍝ collection into single plane.
                  }
             
                  aligned←{                               ⍝ Alignment and centring.
                      rows cols←sepr⍴¨⍵                   ⍝ subarray dimensions.
                      sizes←(⌈/rows)∘.,⌈⌿cols             ⍝ aligned subarray sizes.
                      ctd=0:sizes↑¨⍵                      ⍝ top-left alignment.
                      v h←sepr⌈0.5×↑(⍴¨⍵)-sizes           ⍝ vertical and horizontal rotation.
                      v⊖¨h⌽¨sizes↑¨⍵                      ⍝ centred aligned subarrays.
                  }
             
                  gaps←{                                  ⍝ Gap-separated sub-planes.
                      sep≤⍺≤2:⍵                           ⍝ not separating: done.
                      subs←(⍺-1)∇¨⍵                       ⍝ sub-hyperplanes.
                      width←⊃⌽⍴⊃subs                      ⍝ width of inter-plane gap.
                      fill←(⍺ width-3 0)⍴' '              ⍝ inter-plane gap.
                      ↑{⍺⍪fill⍪⍵}/1 open subs             ⍝ gap-separated planes.
                  }
             
                  plane←{                                 ⍝ Boxed rank-2 plane.
                      sep∧2<⍴⍴⍺:⍺ join ⍵                  ⍝ gap-separated sub-planes.
                      odec←(dec shape ⍺)outer ⍵           ⍝ outer type and shape decoration.
                      idec←inner ⍺                        ⍝ inner type and shape decorations.
                      (odec,idec)collect ⍵                ⍝ collected, formatted subarrays.
                  }
             
                  join←{                                  ⍝ Join of gap-separated sub-planes.
                      sep←(⊃⍴⍵)÷1⌈⊃⍴⍺                     ⍝ sub plane separation.
                      split←(0=sep|⍳⊃⍴⍵)⊂[0]⍵             ⍝ separation along first axis.
                      (⊂[1↓⍳⍴⍴⍺]⍺)plane¨split             ⍝ sub-plane join.
                  }
             
                  outer←{                                 ⍝ Outer decoration.
                      sizes←1 0{⊃↓(⍉⍣⍺)⍵}¨sepr⍴¨⍵         ⍝ row and col sizes of subarrays.
                      sides←sizes/¨¨vt hz                 ⍝ vert and horiz cell sides.
                      bords←dec↓¨ml uc glue¨sides         ⍝ joined up outer borders.
                      ↑,¨/(ul'')⍺ bords(ll ur)            ⍝ vertical and horizontal borders.
                  }
             
                  inner←{                                 ⍝ Inner subarray decorations.
                      deco←{(type ⍵),1 shape ⍵}           ⍝ type and shape decorators.
                      sepr deco¨matr dec open ⍵           ⍝ decorators: tt vv hh .
                  }
             
                  collect←{                               ⍝ Collected subarrays.
                      lft top tt vv hh←⍺                  ⍝ array and subarray decorations.
                      cells←vv right 1 open tt hh lower ⍵ ⍝ cells boxed right and below.
                      boxes←(dec∨0∊⍴⍵)open cells          ⍝ opened to avoid ,/⍬ problem.
                      lft,top⍪↑⍪⌿,/boxes                  ⍝ completed collection.
                  }
             
                  right←{                                 ⍝ Border right each subarray.
                      types←2⊥¨(⍳⍴⍵)=⊂¯1+⍴⍵               ⍝ right border lower corner types.
                      chars←mc mr lc lr[types]            ⍝    ..     ..      ..      chars.
                      rgt←{⍵,(-⊃⍴⍵)↑(⊃⍴⍵)1 1/vt,⍺}        ⍝ form right border.
                      ((matr 1 open ⍺),¨chars)rgt¨⍵       ⍝ cells bordered right.
                  }
             
                  lower←{                                 ⍝ Border below each subarray.
                      bot←{⍵⍪(-1⊃⍴⍵)↑⍺ split ⍵}           ⍝ lower border.
                      split←{((¯2+1⊃⍴⍵)/hz)glue ⍺}        ⍝ decorators split with horiz line.
                      (matr↑,¨/⍺)bot¨matr ⍵               ⍝ cells bordered below.
                  }
             
                  type←{                                  ⍝ Type decoration char.
                      dec<|≡⍵:hz                          ⍝ nested: '─'
                      isor ⍵:'∇'                          ⍝ ⎕or:    '∇'
                      sst←{                               ⍝ simple scalar type.
                          0=dec×⍴⍴⍵:hz                    ⍝ undecorated or scalar ⍕⍵: char,
                          (⊃⍵∊'¯',⎕D)⊃'#~'                ⍝ otherwise, number or space ref.
                      }∘⍕                                 ⍝ ⍕ distinguishes type of scalar.
                      0=≡⍵:sst ⍵                          ⍝ simple scalar: type.
                      {(1=⍴⍵)⊃'+'⍵}∪,sst¨dec open ⍵       ⍝ array: mixed or uniform type.
                  }
             
                  shape←{                                 ⍝ Row and column shape decorators.
                      dec≤0=⍴⍴⍵:⍺/¨vt hz                  ⍝ no decoration or scalar.
                      cols←(×¯1↑⍴⍵)⊃'⊖→'                  ⍝ zero or more cols.
                      rsig←(××/¯1↓⍴⍵)⊃'⌽↓'                ⍝ zero or more rows.
                      rows←(¯1+3⌊⍴⍴⍵)⊃vt rsig'⍒'          ⍝ high rank decorator overrides.
                      rows cols                           ⍝ shape decorators.
                  }
             
                  matr←{↑,↓⍵}                             ⍝ matrix from non-scalar array.
                  sepr←{+/¨1⊂↑⍵}                          ⍝ vec-of-mats from mat-of-vecs.
                  open←{16::(1⌈⍴⍵)⍴⊂'[ref]' ⋄ (⍺⌈⍴⍵)⍴⍵}   ⍝ stretched to expose nulls.
                  isor←{1 ⍬≡(≡⍵)(⍴⍵)}                     ⍝ is ⎕or of object?
                  glue←{0=⍴⍵:⍵ ⋄ ↑⍺{⍺,⍺⍺,⍵}/⍵}            ⍝ ⍵ interspersed with ⍺s.
             
                  isor ⍵:⎕FMT⊂⍵                           ⍝ simple ⎕OR: done.
                  1=≡,⍵:⎕FMT ⍵                            ⍝ simple array: done.
                  box ⍵                                   ⍝ recursive boxing of array.
              }

              display←{⎕IO ⎕ML←0                              ⍝ Boxed display of array.
                  ⍺←⍬ ⋄ ch ⎕PP←2↑⍺,(⍴,⍺)↓1 ⎕PP                ⍝ default chars and precision
                  chars←ch⊃'..''''|-' '┌┐└┘│─'                ⍝ ⍺: 0-clunky, 1-smooth.
             
                  tl tr bl br vt hz←chars                     ⍝ Top left, top right, ...
             
                  box←{                                       ⍝ Box with type and axes.
                      vrt hrz←(¯1+⍴⍵)⍴¨vt hz                  ⍝ Vert. and horiz. lines.
                      top←(hz,'⊖→')[¯1↑⍺],hrz                 ⍝ Upper border with axis.
                      bot←(⊃⍺),hrz                            ⍝ Lower border with type.
                      rgt←tr,vt,vrt,br                        ⍝ Right side with corners.
                      lax←(vt,'⌽↓')[¯1↓1↓⍺],¨⊂vrt             ⍝ Left side(s) with axes,
                      lft←⍉tl,(↑lax),bl                       ⍝ ... and corners.
                      lft,(top⍪⍵⍪bot),rgt                     ⍝ Fully boxed array.
                  }
             
                  deco←{⍺←type open ⍵ ⋄ ⍺,axes ⍵}             ⍝ Type and axes vector.
                  axes←{(-2⌈⍴⍴⍵)↑1+×⍴⍵}                       ⍝ Array axis types.
                  open←{16::(1⌈⍴⍵)⍴⊂'[ref]' ⋄ (1⌈⍴⍵)⍴⍵}       ⍝ Expose null axes.
                  trim←{(~1 1⍷∧⌿⍵=' ')/⍵}                     ⍝ Remove extra blank cols.
                  type←{{(1=⍴⍵)⊃'+'⍵}∪,char¨⍵}                ⍝ Simple array type.
                  char←{⍬≡⍴⍵:hz ⋄ (⊃⍵∊'¯',⎕D)⊃'#~'}∘⍕         ⍝ Simple scalar type.
                  line←{(6≠10|⎕DR' '⍵)⊃' -'}                  ⍝ underline for atom.
             
                  {                                           ⍝ Recursively box arrays:
                      0=≡⍵:' '⍪(open ⎕FMT ⍵)⍪line ⍵           ⍝ Simple scalar.
                      1 ⍬≡(≡⍵)(⍴⍵):'∇' 0 0 box ⎕FMT ⍵         ⍝ Object rep: ⎕OR.
                      1=≡⍵:(deco ⍵)box open ⎕FMT open ⍵       ⍝ Simple array.
                      ('∊'deco ⍵)box trim ⎕FMT ∇¨open ⍵       ⍝ Nested array.
                  }⍵
              }

              dmb←{                                         ⍝ Drop Multiple Blanks.
                  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                     ⍝ nested?
                  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵   ⍝ array
                  2>⍴⍴⍵:(2∨/(~⍵∊⍺),1)/⍵                     ⍝ vector
                  (2∨/(,∨⌿~⍵∊⍺),1)/⍵                        ⍝ matrix
              }

              drvSrc←{⎕IO ⎕ML←0                       ⍝ JS 2012 Guess source of derived function.
             
                  trav←{                                  ⍝ traverse, accumulating subtrees.
                      ~(⊂⍴⍵)∊,¨2 3:leaf ⍵                 ⍝ not a derv or train: done.
                      isop 1⊃⍵:{                          ⍝ derived fn:
                          2=⍴⍵:mop ⍵                      ⍝ monadic operator
                          3=⍴⍵:dop ⍵                      ⍝ dyadic operator.
                      }∇¨⍵                                ⍝ formatted subtrees.
                      leaf ⍵                              ⍝ neither: give up.
                  }
             
                  mop←{1 ⍵}∘{                             ⍝ operator with one operand.
                      (l m)(land oper)←↓⍉↑⍵               ⍝ derived function components.
                      land,oper
                  }
             
                  dop←{1 ⍵}∘{                             ⍝ operator with two operands.
                      (l m r)(land oper rand)←↓⍉↑⍵        ⍝ derived function components.
                      0=r:land,oper,rand
                      land,oper,'(',rand,')'
                  }
             
                  leaf←{0 ⍵}∘{                            ⍝ format leaf.
                      (⊂⍵)∊pfns,pops:⍵                    ⍝ primitive fn/op.
                      (⊂⍵)∊,¨pfns,pops:⍵                  ⍝
                      '{'≡⊃⍵:,⍵                           ⍝ unnamed dfn
                      1∊'←{'⍷1/⍵:(1+(,⍵)⍳'←')↓,⍵          ⍝ dfn←{...} → {...}
                      '⎕'∊⊃⍵:' ',⍵,' '                    ⍝ system fn/op
                      1 1 repObj ⍵
                  }
             
                  isfn←{                                  ⍝ is function?
                      0=≡⍵:⍵∊pfns                         ⍝ primitive function:
                      dfnop ⍵:1                           ⍝ dfn:
                      ~(,¨2 3)∨.≡⊂⍴⍵:0                    ⍝ not a derv or train.
                      isop 1⊃⍵:1                          ⍝ derv.
                      ∇⊃⌽⍵                                ⍝ derv or train.
                  }
             
                  isop←{                                  ⍝ is operator?
                      (⊂⍵)∊pops:1                         ⍝ primitive operator:
                      (⊂⍵)∊'⎕S' '⎕R' '⎕OPT':1             ⍝ system op.
                      ~dfnop ⍵:0                          ⍝ not a d-op:
                      1∊↑'⍺⍺' '⍵⍵'⍷¨⊂⍵                    ⍝ is a d-op.
                  }
             
                  dfnop←{'}'≡⊃⌽~∘' ',⍵}                   ⍝ dfn or dop
             
                  pops←'/\⌿⍀.¨∘⍨&⍣[⌶',⎕UCS(80=⎕DR'')/9056 9016 9060 ⍝ primitive ops.
                  pf0←'+-×÷⌊⌈|*⍟<≤=≥>≠∨∧⍱⍲!?~○'           ⍝ primitive fns (scalar).
                  pf1←'⊢⊣⌷/⌿\⍀∊⍴↑↓⍳⊂⊃∩∪⊥⊤,⍒⍋⍉⌽⊖⌹⍕⍎⍪≡≢⍷'   ⍝ primitive fns (other).
                  pfns←pf0,pf1                            ⍝ primitive fns.
             
                  err←⎕SIGNAL∘11                          ⍝ too hard: give up.
                  ⍺←⊃⎕RSI
                  ⊃⌽trav ⍺{0::⍵ ⋄ ⍺.⎕CR ⍵}⍵               ⍝ source of named function ⍵.
              }

              dtb←{                                         ⍝ Drop Trailing Blanks.
                  ⍺←' ' ⋄ 1<|≡⍵:(⊂⍺)∇¨⍵                     ⍝ nested?
                  2<⍴⍴⍵:(¯1↓⍴⍵){(⍺,1↓⍴⍵)⍴⍵}⍺ ∇,[¯1↓⍳⍴⍴⍵]⍵   ⍝ array
                  1≥⍴⍴⍵:(-+/∧\⌽⍵∊⍺)↓⍵                       ⍝ vector
                  (~⌽∧\⌽∧⌿⍵∊⍺)/⍵                            ⍝ matrix
              }

            ∇ value←fromXML string;mat;shape;⎕ML;type;array;enc;E;n;Chg
     ⍝ Turns an XML string into an APL object
     ⍝ The string must have been produced by the <toXML> fn above.
              ⎕ML←1 ⋄ E←'enclosed' ⍝ for ⊂
          ⍝ Character strings are transformed into a more suitable format
              :Select type←2⊃,mat←'whitespace' 'preserve'⎕XML⍣(0 2∊⍨10|⎕DR string)+string
             
              :CaseList 'array'E
           ⍝ The array could be enclosed several times
                  n←2+enc←+/∧\mat[;2]∊⊂E
                  shape←2⊃⎕VFI 3⊃mat[n;]
                  value←fromXML¨(mat[n;1]=n↓mat[;1])⊂[1]n 0↓mat
                  value←⊂⍣enc⌷shape⍴↑⍣(1∊⍴value)⌷value
              :CaseList 'list' 'number'
                  value←⍬∘⍴⍣(type≡'number')+2⊃⎕VFI 3⊃,mat
              :CaseList 'string' 'char'
                  Chg←(32↑⎕D,⎕A)∘{⍺∊⍨c←⍵.Match[3]:⎕UCS ¯1+⍺⍳c ⋄ ⍵}
                  value←⍬∘⍴⍣(type≡'char')+{'£%.;'⎕R Chg ⍵}3⊃,mat
              :Case 'null'
                  value←⎕NULL
              :EndSelect
            ∇

            fromto←{n←⍴1⊃(t f)←⍺ ⋄ ~∨/b←n≥i←f⍳s←,⍵:s ⋄ (b/s)←t[b/i] ⋄ (⍴⍵)⍴s}

            lcase←'abcdefghijklmnopqrstuvwxyzàáâãåèéêëòóôõöøùúûäæüìíîïðçñ' 'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÅÈÉÊËÒÓÔÕÖØÙÚÛÄÆÜÌÍÎÏÐÇÑ'∘(,⊂' fromto←{n←⍴1⊃(t f)←⍺ ⋄ ~∨/b←n≥i←f⍳s←,⍵:s ⋄ (b/s)←t[b/i] ⋄ (⍴⍵)⍴s}')

            ∇ bcut←la psmum lengths;in;ispacing;iwidth;ittl;cut;n
         ⍝ Find Partition so +/⌈/¨ is under max
              (ispacing ittl)←+\⌽2↑la,1 ⋄ n←in←⍴lengths ⋄ cut←in↑1
              :Repeat ⋄ :Until ∧/bcut←cut
              n←n-1 ⋄ cut←in⍴n↑1
              :OrIf ittl≤+/ispacing+⌈/¨cut⊂lengths
            ∇

              repObj←{             ⍝ String representation of object V0.33
     ⍝ This version accounts for ⎕TC type characters
                  ⍵≡⍬:'⍬' ⋄ ⍵≡'':'''''' ⋄ ⍵≡⎕NULL:'⎕NULL'
                  scal←0∊rank←+/⍴s←⍴⍵ ⋄ char←>/(simple num)←∧\~(10|⎕DR 1/⍵)∊¨6(0 2)
              ⍝ Refs should normally not be displayed. To allow remove set ⍺[2] to 1
                  ⍺←0              ⍝ recursive call?
                  (rc ref)←2↑⍺
                  or←scal∧1=≡R←⍵   ⍝ normally no funny objects like ⎕ORs
                  or∨9=⎕NC'R':ref{⍵⊣÷⍺}⍕⍵     ⍝ display refs as they are
             
     ⍝ Reduce object to 1 item if all same elements
                  mod←(0<rank)∧(n=0)∨(5×char)<n←×/s
                  mod←mod∧as←char{0∊⍴⍵:1 ⋄ ⍵∧.≡1↑⍵}obj←,⍵ ⍝ as: all the same
                  obj←mod{16::⊂'[ref]' ⋄ 1(↑⍣⍺)⍵}obj  ⍝ take only 1st? (grab prototype if empty)
                  shape←mod{⍵≡,1:',' ⋄ (⍺∨1<⍴⍵)/'⍴',⍨⍕⍵}s
                  shape←shape,(encl←simple<as)⍴'⊂'
             
     ⍝ Simple scalars and char vector≠⍴1 do not need parens
                  parens←rc∧simple≤(0<⍴shape)∨(rank=1)∧num∨∨/(4↑⎕AV)∊⍵
                  (lp rp)←parens⍴¨'()'
                  ~simple:rp,⍨lp,shape,encl{⍺⍲'('=1↑⍵:⍵ ⋄ 1↓¯1↓⍵}1↓⊃,/' ',¨{'⎕N'>.=2↑⍵:'(',⍵,')' ⋄ ⍵}¨1 ref∘∇¨obj
             
     ⍝ Simple objects (char should account for ⎕TC chars et al.)
                  ⎕PP←17 ⍝ for numbers
             
                  cmpv←{⎕CT←⎕IO←0                    ⍝ compress numeric vector
                      ∨/e←(0∊s),⍬≡s←⍴v←⍵:⍕e/'⍬',1↑v  ⍝ empty or scalar
                      ⍺←4 ⋄ ⍺≥s:⍕v↑⍨⍺⌊s              ⍝ min length to consider compressing
                      d←{⍵>¯1⌽1 1 0⍷⍵}{1,⍵,⍨1↑⍵}2≠/2-/v
                  ⍝ We know we have at least ONE section to deal with
                  ⍝ We split the list into sections that either have to be compressed or not
                      s←-+/lim←{(⍵>1⌽⍵)∨⍵>¯1↓0,⍵}d   ⍝ where each section starts
                      addp←(s↑1)<s≠¯1                ⍝ where to add parentheses
                      r←1↓⊃,/addp{d v←⍵ ⋄ ∧/d:',',⍕v ⍝ all different?
                          lp←'[('[⍺] ⋄ rp←⍺/')' ⋄ ap←{',',lp,⍵,rp}
                          0=∆←-/2⍴v:ap(⍕⍴v),'/',⍕1⍴v ⍝ all the same
                          A←(nz←0≠t)/⍕t←1⍴v          ⍝ write A+B×⍳C
                          A←A,(nz∨∆>0)/'-+'[∆>0]
                          B←(t≠1)/(⍕t←|∆),'×'
                          C←nz↓'-⎕io-⍳',⍕⍴v
                          (⍴d←⍕v)>⍴t←A,B,C:ap t
                          ',',d
                      }¨↓⍉↑lim∘⊂¨d v
                  ⍝ There may be some superflous commas
                      b←(','=r)⍲(¯1⌽r=')')⍱1⌽r∊'[('
                      (b\b/r)~'['
                  }
             
                  obj←shape,num ⍺{1↑⍺:cmpv ⍵ ⋄ ⎕ML←1 ⋄ ⎕IO←0 ⋄ QU←{Q,((1+t=Q)/t←⍵),Q←''''}
               ⍝ We have to assume not all characters are available. Those should be:
                      Always←⎕A,⎕D,'abcdefghijklmnopqrstuvwxyz_.,:;%!"/=\-+''#$£¢^¿¡(){}[]§@`∣¶&'
                      Always,←'ÁÂÃÇÈÊËÌÍÎÏÐÒÓÔÕÙÚÛÝþãìðòõÀÄÅÆÉÑÖØÜßàáâäåæçèéêëíîïñùúûüóôöø'
                      Always,←'≤≥⌿⍀<>≠∨∧÷×?∊⍴~↑↓⍳○*⌈⌷¨⌊∇∆⍙⍨∘⊂⊃∩∪⊥⊤⌶|⍺⍵¯⍬⍱⍲⍒⍋⍉⌽⊖⍟⌹⍕⍎⍫⍪≡≢⍷⋄←→⍝⎕⍞⍣ '
                      ∧/t←⍵∊Always:QU ⍵ ⍝ no special chars?
                 ⍝ If only a few chars transform the whole string into ⎕AV
                      UCS←{1⌽')(⎕ucs ',⍕cmpv ⎕UCS ⍵}
                 ⍝ More than a few; create a mixture of ⎕UCS and 'quotes'
                      minsize←3 ⍝ how much special chars to include between sections; that number is subjective
                      c∨←minsize>∊⍴¨c←ucs⊂⍨c←1,1↓ucs≠¯1⌽ucs←~t ⍝ consider small groups of ASCII as UCS
                  ⍝ ∧/sc←∊c:ucs ⍵ ⍝ are the pieces small enough to be all in ⎕UCS?
                      (lp rp)←'()'/⍨¨(1↓⍺)∧1<+/c←1,1↓ucs≠¯1⌽ucs←∊c
                      rp,⍨lp,∊{⍺,',',⍵}/(c/ucs){⍺:UCS ⍵ ⋄ QU ⍵}¨c⊂⍵
                  }obj
                  lp,obj,rp
              }

              showCol←{ ⍝ Show table in column format
             ⍝ Each row represents a word
                  max←⌈/w←⍬∘⍴∘⍴¨words←trimEnds¨↓⍣(326≠⎕DR ⍵)⊢⍵
                  0∊⍴words:0 0⍴''
                  ⎕ML←⎕IO←1
             ⍝ We can specify the total width and minimum space between each word (column)
             ⍝ If the minimum is negative, the columns may not be of the same width
                  ⍺←⍬ ⋄ la←{0∊⍴⍵:0 ' ' ⋄ ∨/0 2∊10|⎕DR ⍵:0 ⍵ ⋄ 2↑⍵}⍺
                  la[2]←{3=10|⎕DR ⍵:⍵ ⋄ ⍴,⍵}spacer←2⊃la
                  (spacing width)←{⍵[⍋⍵]}a,(⍳max∧.<a),(max∨.<a←la~0)↓⎕PW
                  0>spacing:0 s↓⊃,/' ',⍣s¨⊢n↑¨↑¨c⊂words⊣n←1⍳⍨1⌽c←⎕PW s psmum w⊣s←-spacing
                  w+←spacing
                  nw←npl×nrw←⌈(⍴w)÷npl←⌊(width+spacing)÷max+spacing
                  words←⍉(npl,nrw)⍴1⌽nw↑(⊂''),(max↑¨words),¨⊂spacing{3=10|⎕DR ⍵:⍺⍴'' ⋄ ⍵}spacer
                  (0,-spacing)↓↑,/words
              }

            ∇ r←{options}showRow words;text;sh;blk;sp;pw;⎕ML;⎕IO;n    ⍝ fit a list ⎕PW wide
              ⍎(0=⎕NC'options')/'options←⍬' ⋄ ⎕ML←⎕IO←1
          ⍝ options are Printing Width, Granularity, min Spacing between words
              (pw blk sp)←options,(⍴,options)↓⎕PW,4 1 ⋄ pw←pw-sp
              sh←⊃,/⍴¨,¨text←{~∘' '¨↓⍵}⍣(326≠⎕DR words)+words ⍝ accept matrix of words
              sh←⊃,/⍴¨text←(blk×⌈blk÷⍨sh+sp)↑¨text ⍝ adjust each word's size
              r←0⍴⊂''
              :While 0<⍴text
                  r←r,,/(n←1⌈+/pw≥+\sh)↑text ⋄ (text sh)←n↓¨text sh
              :EndWhile
              r←↑r
            ∇

            toMatrix←{⍺←3↓4↑⎕AV ⋄ ⎕ML←0 ⋄ (2↑0∊⍴⍵)↓↑1↓¨(s∊⍺)⊂s←(1↑⍺),⍵}

            toVector←{⍺←3↓4↑⎕AV ⋄ b←,1,⌽∨\' '≠⌽⍵ ⋄ 1↓b/,⍺,⍵}

            ∇ string←{sep}toXML value;tag;s;v;⎕PP;repChar;⎕ML;dr;⎕IO
     ⍝ Turns an APL object into an XML representation
     ⍝ Simple scalars are turned into CHAR, NUMBER or NULL
     ⍝ Vectors or rank>1 are turned into ARRAYs of numeric LIST or char STRING
     ⍝ Characters are translated to be acceptable by ⎕XML
              ⎕PP←17 ⋄ ⎕ML←1 ⋄ ⎕IO←0 ⋄ sep←{6::'' ⋄ ⍎⍵}'sep'
              tag←{'<',s,⍵,'</',s←⍺,'>'}
          ⍝ Some characters cannot go thru ⎕XML, this is normal. The valid ones are
          ⍝ #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF]
          ⍝ That is any Unicode character, excluding the control chars, FFFE, and FFFF.
              v←'<>&',⎕UCS s←(⍳32)∩⎕AVU ⍝ this may vary in e.g. Unix Classic
              s←'&lt;' '&gt;' '&amp;','£%'∘,¨(⎕D,⎕A)[s],¨';'
              repChar←s∘(v∘⍳{~∨/b←(⍴⍺)>i←⍺⍺ v←,⍵:⍵ ⋄ (b/v)←⍺[b/i] ⋄ ∊v})
              :If 0∊⍴⍴value ⍝ scalar
                  :If 0∊1↑0⍴value ⋄ string←'number'tag⍕value
                  :ElseIf ' '∊1↑0⍴value ⋄ string←'char'tag repChar value
                  :ElseIf ⎕NULL≡value ⋄ string←'<null/>'
                  :ElseIf 1≥|≡value ⋄ 'no Refs allowed'⎕SIGNAL 11
                  :Else ⍝ must be enclosed
                      string←'enclosed'tag sep toXML↑value
                  :EndIf
              :Else ⍝ not a scalar
                  s←'shape'tag⍕⍴value
                  :If 2|dr←10|⎕DR value ⋄ string←'list'tag⍕,value
                  :ElseIf dr∊0 2 ⋄ string←'string'tag repChar,value
                  :Else
                      string←∊sep∘toXML¨{0∊⍴⍵:⍬⍴⍵ ⋄ ⍵}value
                      :If (1≥×/⍴value)∧1≢≡value ⋄ string←'enclosed'tag string ⋄ :EndIf
                  :EndIf
                  string←sep,'array'tag s,string
              :EndIf
            ∇

            trimEnds←{((∨\b)∧⌽∨\⌽b←' '≠⍵)/⍵}

            ∇ r←la txtreplace string;⎕IO;N;from;to;Fl;Tl;Fp;i;b;ip;n
     ⍝ Text Pattern Replace
     ⍝ la is a 2 elements enclosed strings: what to look for, the replacement string.
     ⍝ Ex: 'abc' 'xxyyzz' will turn all non overlapping 'abc' substrings into 'xxyyzz'
              (Fl Tl)←⍴¨(from to)←,¨la ⋄ Fp←⍴⎕IO←0 ⋄ N←⍴b←from⍷string←,string
              ⎕SIGNAL Fl↓11
              :If 1∊Fl ⋄ Fp←{⍵/⍳⍴⍵}from=string ⍝ simple case
              :Else
                  :While N>i←b⍳1  ⍝ remove overlapping matches
                      (Fl↑(b⍳1)↓b)←0 ⋄ Fp,←i
                  :EndWhile
              :EndIf
             
     ⍝ Do it
              :If 0<ip←⍴Fp
                  :If 0>n←Fl-Tl   ⍝ do we need to insert spaces?
                      b[]←1 ⋄ b[Fp]←1-n ⋄ string←b/string
                      Fp←Fp-n×⍳ip ⍝ adjust positions
                  :EndIf
                  string[,Fp∘.+⍳Tl]←(ip×Tl)⍴to ⍝ replace
                  :If n>0         ⍝ remove excess
                      b[(Fp+Fl-1)∘.-⍳n]←~b[]←1
                      string←b/string
                  :EndIf
              :EndIf
              r←string
            ∇

            ucase←'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÅÈÉÊËÒÓÔÕÖØÙÚÛÄÆÜÌÍÎÏÐÇÑ' 'abcdefghijklmnopqrstuvwxyzàáâãåèéêëòóôõöøùúûäæüìíîïðçñ'∘(,⊂' fromto←{n←⍴1⊃(t f)←⍺ ⋄ ~∨/b←n≥i←f⍳s←,⍵:s ⋄ (b/s)←t[b/i] ⋄ (⍴⍵)⍴s}')

            where←{⍵/⍳⍴⍵}

        :EndNamespace
    :EndNamespace
    :Namespace SALTUtils
⍝ === VARIABLES ===

        AZaz←2 27⍴' ABCDEFGHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz'

        BS←(⎕ucs 8)

        CR←(⎕ucs 13)

        ConfirmEdit←1

        DEBUG←0

        FS←'\'

        LINDEL←13 10

        LastResultVarName←'⎕SE.Dyalog.LastResult'

        MODE←8

        MONITOR←0

        MONITORNAME←'#.UCMDMonitor'

        NewNames←36 2⍴'samplea' 'ucmdnoparsing' 'sampleb' 'ucmdparsing' 'helpexample' 'ucmdhelp' 'cputime' 'runtime' 'efa' 'fileassociations' 'aedit' 'array.edit' 'factorsof' 'factors' 'ffind' 'find' 'fncalls' 'calls' 'freplace' 'replace' 'fto64' 'tolarge' 'ftttots' 'toquadts' 'svnci' 'commit' 'splitfile' 'split' 'wspeek' 'peek' 'wsdoc' 'documentation' 'cfcompare' 'file.compare' 'commentalign' 'align' 'fncompare' 'fn.compare' 'reordlocals' 'reorderlocals' 'urefresh' 'refresh' 'supdate' 'scriptupdate' 'varcompare' 'array.compare' 'wscompare' 'ws.compare' 'wslocate' 'locate' 'uclean' 'clean' 'svnco' 'checkout' 'uversion' 'version' 'svnadd' 'add' 'svndelete' 'delete' 'svndiff' 'diff' 'svnexport' 'export' 'svnimport' 'import' 'svnresolve' 'resolve' 'svnstatus' 'status' 'svnupdate' 'update'

        NewObjectsFolder←''

        PATHDEL←';∘'

        SALTEXT←'.dyalog'

        SETCOMPILED←0

        SETTS←0

        UCauto←1

        UVersion←2.11

        V14←1

        WIN←1

        _←36 2⍴'samplea' 'ucmdnoparsing' 'sampleb' 'ucmdparsing' 'helpexample' 'ucmdhelp' 'cputime' 'runtime' 'efa' 'fileassociations' 'aedit' 'array.edit' 'factorsof' 'factors' 'ffind' 'find' 'fncalls' 'calls' 'freplace' 'replace' 'fto64' 'tolarge' 'ftttots' 'toquadts' 'svnci' 'commit' 'splitfile' 'split' 'wspeek' 'peek' 'wsdoc' 'documentation' 'cfcompare' 'file.compare' 'commentalign' 'align' 'fncompare' 'fn.compare' 'reordlocals' 'reorderlocals' 'urefresh' 'refresh' 'supdate' 'scriptupdate' 'varcompare' 'array.compare' 'wscompare' 'ws.compare' 'wslocate' 'locate' 'uclean' 'clean' 'svnco' 'checkout' 'uversion' 'version' 'svnadd' 'add' 'svndelete' 'delete' 'svndiff' 'diff' 'svnexport' 'export' 'svnimport' 'import' 'svnresolve' 'resolve' 'svnstatus' 'status' 'svnupdate' 'update'

        cDesc←4

        cFullName←7

        cGroup←1

        cLname←2

        cMinLen←8

        cName←3

        cObjName←6

        cParse←5


⍝ === End of variables definition ===

        (⎕IO ⎕ML ⎕WX)←1 3 3

        ∇ loc BootLib target;file;name;t;tgt
     ⍝ Bring in a single Unicode file
          file←loc,'core',(¯1↑loc),target{⍵,⍨(-⊥⍨'.'≠⍺)↑⍺}'.dyalog'
          t←splitOnNL file
     ⍝ The name of the object produced MUST match the target name:
          (tgt name)←'.'splitLast target
          tgt←⎕SE⍎tgt,(0∊⍴tgt)/'##'
          {}÷target≡4↓⍕name←tgt.⎕FIX t
          name.SALT_Data←⎕SE.⎕NS''
          name.SALT_Data.(SourceFile LastWriteTime Version)←file'(unavailable)' 0
        ∇

        BootPath←{(p↓⍨-'/\'∊⍨¯1↑p←getEnvir'SALT'),FS,⍵}

        ∇ BootSALT;bl
     ⍝ Bring in all necessary code to run SALT if enabled
          '⎕se.Dyalog'⎕NS''               ⍝ make sure this one is there
          bl←BootPath''
          bl BootLib'Dyalog.Utils'
          bl BootLib'Parser'
          bl BootLib'UnicodeFile'         ⍝ unused by SALT itself but used by other code
          bl BootLib'SALTUtils'
          bl BootLib'SALT'
          ⎕SE.Dyalog.SEEd←⎕SE.⎕WG'Editor'
          ⎕SE.Dyalog.SEEd.⎕WX←3           ⍝ use this to store the callbacks
          ⎕SE.SALTUtils.EditorFix'Start'  ⍝ set callback in the NEWly defined ns
          ⎕SE.⎕FX ⎕CR'UCMD'               ⍝ define <UCMD> fn
         
          ⎕EX'⎕se.Dyalog.SALT.List'       ⍝ ensure UCMDs cache location empty
         
          ⍝ We don't initialize the UCMD list if we're starting and this is not a Windows version
          ⎕SE.SALTUtils.CMDDIR←BootSpice
          :If ∨/'0nN'∊getEnvir'SALT\AddSALT'
              ⍝ Ensure editor callback is off if SALT is NOT enabled
              :Trap 0
                  1 ⎕SE.SALTUtils.EditorFix'End'
              :EndTrap
          :EndIf
         
     ⍝ Check for autostart (dyapp= on command line)
          :If 0≠⍴bl←getEnvir'dyapp'
              ⎕←'Booting ',bl
              ⎕SE.SALT.Boot{Q,⍵,Q←1↑'"'''~⍵}bl
          :EndIf
          {0:: ⋄ ⎕SIGNAL 0}''       ⍝ reset ⎕DM
        ∇

        ∇ {cd}←BootSpice
         ⍝ Reset UCMD cache
          :If ~0∊⍴cd←ResetUCMDcache 1
              Spice'usetup init'
          :EndIf
        ∇

        ∇ {r}←CC list;all;b ⍝ Code Coverage. list is the fns to set
          ⍝ or 1 to initialize all or 0 (stop & clear) or -1 (stop)
          :Access public shared
          :Trap 6
              {}CodeCov
          :Else
              ⎕SE.SALT.Load'tools\code\codecov'
          :EndTrap
          all←⎕NL-3.1 4.1
          :If 1∊b←¯1 0∊list
              r←CodeCov.CC CCdata,{(⎕MONITOR ⍵)[;2]}¨CCdata[;1]
              →b[1]/0 ⍝ do not reset numbers if ¯1
              →0,⍬∘⎕MONITOR¨all
          :ElseIf list≡1
              list←all~'CC' 'UCMD'
          :EndIf
          r←CCdata←''CodeCov.CCinit⍨⎕CR¨⊂⍣(2>≡list)⊢list
          (¯1+⍳999)∘⎕MONITOR¨list ⍝ we can't do this inside CCinit
        ∇

        ∇ {old}←CD new;get;set;size;t                             ⍝ Change directory.
          :Trap 0
              old←¯1↓1⊃1 ⎕NPARTS''                                ⍝ try new fn first
              old[t/⍳⍴t←old∊WIN/'/']←'\'
              →(⍴new)↓0
          :EndTrap
          :If ~WIN
              old←,⊃⎕SH'pwd' ⍝ ignore set for the nonce
          :Else
              t←'A*'[1+80=⎕DR'']
              'get'⎕NA'U Kernel32|GetCurrentDirectory',t,' U >0T' ⍝ Associate Get function.
              'set'⎕NA'U Kernel32|SetCurrentDirectory',t,' <0T'   ⍝     "     Set    "
              (size old)←get 260 260                              ⍝ Get current directory.
              :If ×⍴,new                                          ⍝ If target directory.
                  ⎕SIGNAL(set⊂new)↓11                             ⍝ Domain error if fails.
              :EndIf                                                ⍝ Old dir is shy rslt.
          :EndIf
        ∇

        ∇ folder←root ClassFolder folder;and;nocl;rooted;rp;rs;special;stem
     ⍝ Produce full path by merging root and folder name
          :Access Shared Private
     ⍝ [xxx] in folder names resolved using GetEnvironment including [DYALOG]
          folder↓⍨←-FS=¯1↑folder                  ⍝ remove last \
          →((rp≢FS,'*')∧≠/(FS,':')=rp←2↑folder)⍴0 ⍝ is it already formatted properly?
          and←{((-FS=¯1↑⍺)↓⍺),(0<⍴⍵)/FS,('/\'∊⍨1↑⍵)↓⍵}
          special←''
         
     ⍝ Special names treated here
          :If '['=1↑folder
              folder←0/root←folder
          :EndIf
          :If '['=1↑root
              special←specialName(rp←root⍳']')↑root
              folder←special and(rp↓root)and folder
          :Else
              folder←root and folder
          :EndIf
        ∇

        ∇ dstname CopyNs source;b;cls;dref;fn;name;ns;nss;s;scrref;src;val;vars;t
     ⍝ Copy contents of a scripted NAMESPACE into a new non scripted one
     ⍝ This fn works 2 ways:
     ⍝ 1: with a source reference which is to be recreated wo source to the target
     ⍝ 2: with a source NAMED which is NOT to be recreated if its display form does not match
          :If 2=⍴,scrref←source
              name←⍕scrref←⍎/source
              →0/⍨name≢(⍕1⊃source),'.',2⊃source ⍝ skip refs
          :EndIf
          ⎕EX dstname         ⍝ ensure not there
          dref←⍎dstname ⎕NS'' ⍝ create new namespace & get its reference
          dref.(⎕IO ⎕ML ⎕WX ⎕USING ⎕CT)←scrref.(⎕IO ⎕ML ⎕WX ⎕USING ⎕CT) ⍝ ⎕vars
          vars←scrref{0∊⍴⍵:0 2⍴0 ⋄ ⍵,[1.5]⍺.⍎¨⍵}scrref.⎕NL-2
          :For name val :In ↓vars
              ⍎dstname,'.',name,'←val'
          :EndFor
          :If ∨/b←dref.{11::1 ⋄ 0∊1↑0⍴⎕FX ⍵}¨src←scrref.(⎕NR¨⎕NL-3 4)
     ⍝ OK, some fn didn't fix, probably because it is a ref of some sort
     ⍝ If it happens to be made with ∘ there is a chance it was with ','
     ⍝ and the result MAY be wrong, we chance it:
              name←b/scrref.⎕NL-3 4 ⋄ src←b/src
              ⍝ Find the definition in the source
              val←⎕SRC scrref
              :For fn s :InEach name src  ⍝ we can't do them all at once
                  :If 1∊⍴t←('\b',fn,'←[^⋄]*')⎕S'&'⊢val ⍝ only ONE match?
                      dref⍎1⊃t ⍝ apply verbatim
                  :Else ⍝ take a chance
                      dref⍎fn,'←',⍕s
                  :EndIf
              :EndFor
          :EndIf
          :For cls :In scrref.⎕NL-vars←9.5 ⍝ Interfaces
              :If scrref isReal cls
                  dref.⎕FIX ⎕SRC scrref⍎cls
              :EndIf
          :EndFor
     ⍝ Naive redefinition: if base classes are present this will fail
          :For cls :In scrref.⎕NL-9.4
              :If scrref isReal cls
                  dref.⎕FIX ⎕SRC scrref⍎cls
              :EndIf
          :EndFor
          :For ns :In (scrref.⎕NL-9.1)~⊂'SALT_Data' ⍝ Namespaces
              :If scrref isReal ns
                  (dstname,'.',ns)CopyNs(scrref ns)
              :EndIf
          :EndFor
          ⍝
        ∇

        DF←{(⍵.⎕DF df){⍵}⍕⍵{⍺}df←⍵.⎕DF ⎕NULL}

        ∇ _nam Default _val
          :Access Shared Private
          ⍎(0=⎕NC _nam)/_nam,'←_val'
        ∇

        ∇ files←{parms}Dir path;dfa;switches;⎕IO;viz
     ⍝ List directory using DOS DIR command
     ⍝ parms may be any of
     ⍝ dD: list dirs only
     ⍝ fF: list files only
     ⍝ aA: list all data (timestamp, size, name)
          :Access Shared Private
          'parms'Default'' ⋄ ⎕IO←1
          path←path,(FS∊¯1↑path)/'*' ⍝ ensure * after /
          :Trap 2 22
              viz←0=5⊃files←1 3 2 0 6 ⎕NINFO ⎕OPT 1∊path←1 ⎕NPARTS path
              files←viz∘/¨¯1↓files
              files[1]=←1
              files[4]↓¨⍨←≢1⊃path
          :Case 22
              files←4⍴⊂⍬
          :Case 2
              files←WIN NtDirX{⍺:⍺⍺ ⍵ ⋄ ⍵⍵ ⍵}DirU path
          :EndTrap
          :If ∨/2↑dfa←∨/3 2⍴'dDfFaA'∊parms
              files←((⎕IO⊃files)=1↑dfa)∘⌿¨files
          :EndIf
          :If 0=3⊃dfa
              files←4⊃files
          :EndIf
        ∇

        ∇ files←DirU x;b;cut;dir;list;none;⎕IO;⎕ML
     ⍝ Unix ls: drop 1st line?, cut into sections
          ⎕IO ⎕ML←1 0
          files←0⍴¨0 '' 0 '' ⋄ none←1
          :Trap 11          ⍝ drop . with ls -l:long, d:directories, H:follow symbolic links
              none←0∊⍴list←' ',↑{('total'≡5⍴↑⍵)↓⍵}⎕SH'ls -ldH ',x,' 2>/dev/null'
          :EndTrap
          →none⍴0
          b←cut⍲1⌽cut←∧⌿' '=↑list
          dir←list[;2]='d'
          files←1⊃⌽list←0 1∘↓¨⊃⊂/b∘/¨cut list
          files←{(-⊥⍨' '=⍵)↓⍵}¨↓(⌽∧\FS≠⌽1⌷[1]files⍪' ')/files
     ⍝ 1st section should be permissions, last is name
          files←dir(↓⍕5↓¯1↓list)(2⊃⎕VFI,' ',5⊃list),⊂files
        ∇

        ∇ {bypasschk}EditorFix args;df;file;fixed;i;id;name;nc;ns;nss;r;sd;ss;t;target;⍙
          :Access Shared Private
         ⍝ Callback fn for the editor
          :If ~{6::⍵ ⋄ bypasschk}0
              →0/⍨∨/'0nN'∊getEnvir'SALT\AddSALT' ⍝ has SALT been disabled?
          :EndIf
          :If 1≡≡args
              args←0 args   ⍝ pad to get min 2
          :EndIf
         
          :If DEBUG>1
              ⎕←{6::⍵ ⋄ (⊂t[2].SourceFile),t←⎕SE.Dyalog.⍙}'No ⍙'
          :EndIf
         
          :Select t←2⊃args
         
          :Case 'Fix' ⍝ editor callback: [3] source, [4] ns, [5] name
     ⍝ When the editor exits we grab its SALT data for the AfterFix event
     ⍝ and we store it in ⎕se.Dyalog along with source ID
     ⍝ We can edit something IN a class.
     ⍝ In that case the class will be reported as being modified.
              nss←⍴¨ss←⊂'⍝SALTSource="'
              (target name)←args[4 5]
              :If ∨/3 4∊nc←⌊|target.⎕NC⊂name⊣id←(i←DF target),'.',name
                  :If ~0∊⍴t←fnData args[3 5]            ⍝ SALTed fn?
                      ⎕SE.Dyalog.⍙←id t(target.⎕STOP name)
                      →0
                  :EndIf
                  ⍝ If it's not SALTed maybe its parent is
                  nc←9 ⋄ name←i ⋄ target←target.##
              :EndIf
              :If 2∊nc
                  →0/⍨0∊⍴t←varData⊂target name          ⍝ SALTed var?
                  ⎕SE.Dyalog.⍙←id t 0
              :ElseIf nc=9   ⍝ did we edit something IN a SALTed class?
                  :If 9=target.⎕NC ns←name,'.SALT_Data' ⍝ NS which has a SALT_Data
                  :AndIf ≡/args[5 6]                    ⍝ same name?
                      t←target.⎕OR ns                   ⍝ grab a copy
                      df←⍕target⍎name                   ⍝ and its display form
                      ⎕SE.Dyalog.⍙←id(⎕NS t)df          ⍝ being careful not to keep the original
                  :ElseIf (⍴t)≥i←(nss↑¨t←{0::⍬ ⋄ ⎕SRC target⍎⍵}name)⍳ss
                      ⍙←⎕NS''
                      ⍙.SourceFile←{(¯1+⍵⍳'"')↑⍵}{(⍵⍳'"')↓⍵}i⊃t
                      ⍙.Version←0
                      ⍙.LastWriteTime←⍕lastWrTime ⍙.SourceFile
                      ⍙.GlobalName←''
                      ⎕SE.Dyalog.⍙←id ⍙ 0
                  :EndIf
              ⍝ New object. Are we tracking them?
              :ElseIf ~0∊⍴NewObjectsFolder
                  ⎕SE.Dyalog.⍙←id 0 0     ⍝ new object signature
              :EndIf
         
          :Case 'AfterFix'
     ⍝ After an object has been fixed successfully we add the SALT details if any
     ⍝ which we stored in ⎕se.Dyalog.⍙
              target←fixed←df←0
              :If 2=⎕NC sd←'⎕se.Dyalog.⍙' ⍝ they were kept in this global in this ns
              :AndIf (,3)≡⍴t←⍎sd          ⍝ we need a specific signature
              :AndIf ((DF target←4⊃args),'.',5⊃args)≡1⊃(id ⍙ df)←t
                  ⍝ If ⍙ is 0 it's a new object
                  :If 0≡⍙
                      ⎕←'* New object ',id,': ',⎕SE.SALT.Save id,' ',NewObjectsFolder,'/'
                  :Else
     ⍝ If ⍙ contains 'Name', it represents the name of the pgm fixed if it was the case.
     ⍝ If there is no such name then a class was fixed. Or it could be new.
                      :If 0=⍙.⎕NC'Name' ⍝ is it a class in a scripted ns?
                     ⍝ ns in non scripted nss are OK
                          :If 9=target.⎕NC 5⊃args
                              target←target⍎5⊃args
                          :EndIf
                          target.SALT_Data←target.⎕NS ⎕OR'⍙' ⍝ needed in 12.1
                      :ElseIf ≢/args[5 6]       ⍝ changed a program name?
                      :AndIf (target.⎕NC 5⊃args)∊3 4
                          target.⎕FX remTag target.⎕NR 6⊃args ⋄ →0⊣⎕EX sd ⍝ remove its tag
                      :EndIf
                      fixed←Fixed target ⍙
                      :If 0 2∊⍨10|⎕DR df
                          {}target.⎕DF df       ⍝ restore ⎕DF if was present
                      :EndIf
                  :EndIf
              :EndIf
              ⎕EX sd ⍝ prevent accidents
              →L10/⍨fixed∧'⎕SE.SALTUtils'≡⍕target
         
          :CaseList 'Start' 'chk'
         
         L10: ⍝ It looks as if keeping a link to the editor outside
          ⍝ of this ns solves problems with callback settings dissapearing
              :If 0=⎕SE.Dyalog.⎕NC'SEEd'
                  ⎕SE.Dyalog.SEEd←⎕SE.⎕WG'Editor'
              :EndIf
              :If t≢'Start'
                  ⎕←⎕SE.Dyalog.SEEd.⎕WG'Event'
              :EndIf
              ⎕SE.Dyalog.SEEd.⎕WS'Event'('Fix' 'AfterFix')(⎕IO⊃⎕XSI)
         
          :CaseList 'End' 'Stop'
              ⎕SE.Dyalog.SEEd.⎕WS'Event'('Fix' 'AfterFix')0
         
          :EndSelect
        ∇

        ∇ rslt←Filetime_to_TS filetime;⎕IO
          :Access Shared Private
          :If 1≠0⊃rslt←FileTimeToLocalFileTime filetime(⎕IO←0)
          :OrIf 1≠0⊃rslt←FileTimeToSystemTime(1⊃rslt)0
              rslt←0 0                   ⍝ if either call failed then zero the time elements
          :EndIf
          rslt←1 1 0 1 1 1 1 1/1⊃rslt    ⍝ remove day of week
        ∇

        ∇ FindDefine;T;WIN32_FIND_DATA
          :Access Shared Private
          T←'A*'[80∊⎕DR'']
          WIN32_FIND_DATA←'{I4 {I4 I4} {I4 I4} {I4 I4} {U4 U4} {I4 I4} T[260] T[14]}'
          'FindFirstFileA'⎕NA'P kernel32.C32|FindFirstFile',T,' <0T >',WIN32_FIND_DATA
          'FindNextFileA'⎕NA'U4 kernel32.C32|FindNextFile',T,' P >',WIN32_FIND_DATA
          ⎕NA'kernel32.C32|FindClose P'
          ⎕NA'I4 kernel32.C32|FileTimeToLocalFileTime <{I4 I4} >{I4 I4}'
          ⎕NA'I4 kernel32.C32|FileTimeToSystemTime <{I4 I4} >{I2 I2 I2 I2 I2 I2 I2 I2}'
          ⎕NA'I4 kernel32.C32∣GetLastError'
        ∇

        ∇ rslt←FindFirstFile name;⎕IO;N
          :Access Shared Private
          rslt←FindFirstFileA name(⎕IO←0)
          :If 1∊(¯1+2*32 64)=0⊃rslt       ⍝ INVALID_HANDLE_VALUE 32 or 64
              rslt←0 GetLastError
          :Else
              (1 6⊃rslt)trimAt←N←⎕UCS 0   ⍝ shorten the file name at the null delimiter
              (1 7⊃rslt)trimAt←N          ⍝ and for the alternate name
          :EndIf
        ∇

        ∇ rslt←FindNextFile handle;⎕IO;N
          :Access Shared Private
          rslt←FindNextFileA handle(⎕IO←0)
          :If 1≠0⊃rslt
              rslt←0 GetLastError
          :Else
              (1 6⊃rslt)trimAt←N←⎕UCS 0   ⍝ shorten the filename
              (1 7⊃rslt)trimAt←N          ⍝ shorten the alternate name
          :EndIf
        ∇

        FixWOtag←{orig≡rep←remTag⊢orig←⍺.⎕NR ⍵:0 ⋄ 11::1 ⋄ 1⊣at(⍺.(1159⌶))⍺.⎕FX rep⊣at←⍺.⎕AT ⍵}

        ∇ done←Fixed(ns ⍙);buttons;confirm;ext;fmt;here;isfn;isns;key;last;maxv;mode;n;name;new;nofile;path;prev;r;source;sourcefile;s0;t;tsinfo;upd;version;ync;z
          :Access Shared Private
         ⍝ Called after object has been fixed to update the source file
          nofile←1 ⋄ isfn←done←0 ⋄ z←,⊂'' ⋄ fmt←0
          :If isns←0∊⍙.⎕NC'Name' ⍝ Class? Namespaces have no 'Name' var
              source←⎕SRC ns ⋄ name←DF ns
          :ElseIf isfn←4 3∨.=ns.⎕NC name←⍙.Name
              source←remTag ns.⎕NR name  ⍝ remove tag line for fns
          :Else ⍝ it's a variable, hopefully palatable
              fmt←⍙.Format ⋄ source←name(fmt≡'xml')∆VCR{1=≡⍵:⍎⍵ ⋄ ⊃⍎/⍵}⍙.Pathname
          :EndIf
          :Trap 22 ⍝ the file may be gone
              key←{6::0 ⋄ ⍵.EKey}⍙
              z←fixTabs key splitOnNL ⍙.SourceFile ⋄ nofile←0
          :EndTrap
          :If ('<APLScript>'startsWith⍨1⊃z)∧':Class PageClass'startsWith⍨1⊃source
              source←z MakeAPLScript source
          :EndIf
         
          :If source≢z ⍝ Any changes?
          :AndIf (rlb¨source)≢rlb¨z ⍝ we shouldn't have to do this
         
     ⍝ This is an update; there are 2 cases:
         
     ⍝ 1. we are adding a new version: we use the next available number
     ⍝  We have to be careful not to add a new version from an outdated version (e.g. we loaded an
     ⍝  old ws containing an outdated version). For this we check that the version # is exactly the
     ⍝  same as the highest verno. In the case where we actually requested to reload an old version
     ⍝  it will be negative to denote that fact (i.e. Load will negate the verno)
     ⍝  In the case where a brand new version is added (we edited the very LAST version) the prompt includes
     ⍝  an option to overwrite the last version.
         
     ⍝ 2. we are replacing an existing file: we confirm if confirmation not disabled
     ⍝  If we attempt to overwrite with an old version we will detect it by comparing the timestamps
         
              z←s0←sourcefile←⍙.SourceFile
              :If new←last←0≠version←maxv←⍙.Version ⍝ case #1
                 ⍝ There should be a version # but just in case...
                  (t n ext)←splitName 2⊃(path t)←FS splitLast sourcefile
                  maxv←⌈/0,path ListVersions t,'.',ext ⍝ find last verno
                  last←version≥maxv ⍝ if it is > it must be because we used RemoveVersions
                  version←1+maxv
                  (s0 sourcefile prev)←(path,FS,t)∘,¨((⊂''),'.',¨⍕¨1 0+maxv),¨⊂'.',ext
              :EndIf
         
              ⍝ We can automate confirmation by setting 'ConfirmEdit' to be 1 (YES)
              ⍝ The 2nd element of 'ConfirmEdit', is present, is the answer to the following
              ⍝ 3 questions.
              :If ~1↑(confirm ync)←2↑ConfirmEdit,1  ⍝ has confirmation been disabled?
                  ync←3⍴ync ⍝ used for answering the next 3 questions automatically
                 ⍝ Confirmation required, prepare the question
                  upd←⊂('Update' 'Create a new'⊃⍨1+new),' source file for "',name,'"?'
                  t←(nofile<new>last)/'NOTE: *** this is NOT the latest version!'
                  t←t,nofile/'NOTE: *** the original file is no longer found!'
                  mode←'overwritten.' 'created.'⊃⍨1+new∨nofile
                  t←t('If you choose YES, file "',sourcefile,'" will be ',mode)''
                  buttons←'Yes' 'No',last/⊂'Cancel'
                  :If last←last>nofile
                      t,←⊂'If you choose NO, file "',prev,'" will be rewritten'
                      t,←'' 'If you choose CANCEL, the changes won''t be filed'
                  :Else
                      t,←⊂'If you choose NO, the changes won''t be filed'
                  :EndIf
              :OrIf (2+last)>ync←msgBox('You have modified ',⍕name)(upd,t)'Query'buttons
         
                  :If confirm<×DEBUG
                      ⎕←'* auto response to "You have modified...":',1↑ync
                  :EndIf
                  :If last∧2∊1↑ync
                      sourcefile←prev ⋄ version←maxv
                  :EndIf
         
                 ⍝ Make sure the file is not stale.
                 ⍝ NOTE: we consider the file up to date if the version is > the one on disk
                 ⍝ which may happen after running REMOVEVERSIONS
                  :If (maxv>n)∧0<n←⍙.Version
                      :If confirm
                          t←('This is version ',(⍕n),', the latest is ',⍕maxv)('Do you still want to save it as V',(⍕maxv+1),'?')
                          →0⍴⍨1≠msgBox'*** THIS IS NOT THE MOST RECENT VERSION !!!'t'Warn'
                      :Else
                          :If ×DEBUG
                              ⎕←'* auto response to "This is version X":',t←1↑1↓ync ⋄ →0/⍨t≠1
                          :EndIf
                      :EndIf
                 ⍝ File is not new: ensure it hasn't been updated outside
                  :ElseIf ~nofile
                  :AndIf ⍙.LastWriteTime{⍺≢⍵:6<⍴⍺∪⎕D ⋄ 0}t←⍕lastWrTime ⍙.SourceFile   ⍝ not same as we loaded?
                     ⍝ If no timestamp is found the file is probably gone, all we need to do is rewrite (with permission)
                     ⍝ If its folder is also gone we can't do it and we simply tell the user when we fail to write the file.
                      :If confirm
                          t←{0∊⍴⍵:'No timestamp found for original file!' ⋄ 'Now dated ',⍵}t
                          t←⍙.SourceFile''('Was dated ',⍙.LastWriteTime,' when loaded...')(t)'Proceed anyway?'
                          →0⍴⍨1≠msgBox'Source file timestamp has changed...'t
                      :Else
                          :If ×DEBUG⊣t←¯1↑ync
                              ⎕←'* "Was dated" auto response:',t
                          :EndIf
                          →0/⍨t≠1
                      :EndIf
                  :EndIf
                  tsinfo←⍬
                  ⍝ Insert TS/AN info
                  :If (SETCOMPILED∨SETTS)∧~1∊'/SALT/'⍷uCase{s←'\'=v←⍵ ⋄ (s/v)←'/' ⋄ v}sourcefile
                      :If isfn
                          tsinfo←ns getTs,⊂name
                      :ElseIf isns
                          tsinfo←ns getTs⍣(~0∊⍴t)+t←ns.⎕NL ¯3.1
                      :EndIf
                  :EndIf
                  z←mergeTxt('⍝:Pragma Line 1'startsWith⍨1⊃source)↓source,tsinfo
                  :Trap 22
                      z PutUTF8File(t←sourcefile)key
                      :If new
                          z PutUTF8File(t←s0)key
                      :EndIf
                  :Else
                      ⎕←'*** Unable to save "',t,'", SALT tag is removed'
                      ⎕←'*** Use ]SAVE to save the object manually'
                      :If isfn
                          ns.⎕FX source
                      :ElseIf isns
                          ns.⎕EX'SALT_Data'
                      :Else
                          1 varData⊂ns name
                      :EndIf
                      →0
                  :EndTrap
                  done←1
                  :If isns
                      ⍙←ns.SALT_Data ⍝ shorthand
                      ⍙.LastWriteTime←⍕lastWrTime ⍙.SourceFile←sourcefile ⍝ update Timestamp
                      ⍙.(Version EKey)←version key
                      :If DEBUG>1
                          ⎕←'* New Tag info: ',⍙.(LastWriteTime SourceFile Version EKey)
                      :EndIf
                  :Else
                      SetDelta(ns name source,isfn/⊂tsinfo)sourcefile version ⍬ fmt key
                  :EndIf
              :EndIf
          :EndIf
        ∇

          Fold←{s←1≡≡⍵ ⋄ l←s{⍺:¯2-/0,b/⍳⍴b←1,⍨' '=⍵ ⋄ 2+∊⍴¨⍵}⍵
              cut←(a↑1)⌹((2⍴a)⍴(1+a←⍴l)↑1)-<⍀b∘.>⍺+¯1↓0,b←+\l
              cut←{¯1↓∊l↑¨⍵}⍣s+cut
              ⎕ML←1 ⋄ cut⊂⍵}

        ∇ {rc}←Forget(folder name version keeplast newlast noprompt);dotVer;ext;f;last;list;next;nf;ok;prev;S;stem;t;Ver
     ⍝ Forget Backup Versions
          :Access Shared Private
          ok←1 ⋄ dotVer←{(0<⍵)/'.',⍕⍵} ⋄ S←{(1=⍴⍺)↓'s',⍵} ⍝ add 's' to plural
          :If 12<nf←⍴t←list~keeplast/⌈/list←{⍵[⍋⍵]},version
              t←t[1],(⊂'...'),¯1↑t
          :EndIf
          (stem f ext)←splitName name ⋄ Ver←'Version',t S' ',⍕fmtVersion¨t ⋄ ext←'.',ext
          :If 0<nf
              t←('Forget Versions of ',name)(('Confirm deletion of ',(⍕nf),' version',list S':')''Ver)'Warn'
          :AndIf ok←t{⍵:1 ⋄ 1=msgBox ⍺}noprompt
              :For t :In list
                  fileErase f←(folder,FS,stem,dotVer t),ext
              :EndFor
     ⍝ This should go to the stderr stream
              ⎕←(⍕nf),' version',list S' deleted.'
          :EndIf
          →ok↓0×rc←nf⌈⍣ok-1 ⍝ return -1 if user aborted
     ⍝ If a new last version results of the deletions we must make a copy to the non-versioned file
          :If newlast∨.<version ⍝ then the old current version has been deleted
          :AndIf keeplast∨newlast>0
              f←folder,FS,stem,ext ⋄ last←(folder,FS,stem,dotVer newlast+keeplast),ext
              fileCopy/keeplast⌽last f
          :EndIf
        ∇

        ∇ (cmddir list)←{tell}GetUCMDList names;folder;nc;ns;show1;t;b;gn;cn;files
         ⍝ Retrieve the list of all Spice commands
          :If show1←326∊⎕DR names
              folder←1↑(cmddir names)←names
          :Else
              folder←ClassFolder∘''¨(cmddir←⎕SE.SALT.Settings'cmddir')splitOn PATHDEL
          :EndIf
          t←↑⍪/⎕SE.SALT.List¨'"',¨folder,¨⊂FS,names,'" -rec -raw -full=2'
          files←∪(0=,⊃⍴¨t[;1])/t[;2]
          :If ~show1
              files∪←(BootPath'spice',FS)∘,¨'Spice' 'SaltInSpice' 'NewCmd'  ⍝ always there
          :EndIf
          list←⍬
          :If 0=⎕NC'tell'
              tell←0
          :EndIf
     ⍝ Spice keeps track of the commands in the Spice folder
          :For t :In files
              :Trap DEBUG↓0
                  ns←⎕SE.SALT.Load'"',t,'.dyalog" -noname -nolink'
                  :If 9=⎕NC'ns'
                  :AndIf 3=⌊|ns.⎕NC⊂'List'
                  :AndIf ~0∊⍴nc←ns.List
                      nc←{⍵⊣⍵.(Name Group)←'' 'NONE'VerifyNEstring¨2↑⍵.(Name Group Desc Parse)←,¨⍵.(Name Group Desc Parse)}¨,nc
                      nc.ObjName←⊂{⍵↑⍨-⊥⍨'.'≠⍵}⍕ns
                      nc.FullName←⊂t
                      nc.Name←~∘'()'¨cn←nc.Name
                      nc.MinLen←{(b⍳1)×1∊b←'('=⍵}¨cn
                      list,←nc
                  :EndIf
              :Else
                  tell{⍺:⎕←⍵}'Error loading a User Command from "',t,'": ',⎕IO⊃⎕DM
              :EndTrap
          :EndFor
          →(⍴list)↓0
         ⍝ Remove duplicates, if any. Groups are important.
          list←(b←(⍳⍴t)=t⍳t←{⍺,'.',⍵}/gn←lCase¨⊃list.(Group Name))/list ⋄ t←b/t ⋄ gn←b⌿gn
         ⍝ At this point all we have is unique group/names. We now try to find the unique names
          list←gn,⊃list.(Name Desc Parse ObjName FullName MinLen) ⍝ get rid of namespaces
          list[;8]←list[;8]findMinLen gn
        ∇

        ∇ r←{remove}GetUnicodeFile ra;b;clean;file;key;nc;tn;utf8;ucs2;from;to;transform;⎕TRAP
     ⍝ Read a Unicode (UTF-8 or even UCS-2) file
     ⍝ This version allows excluding specific 1-byte characters before the translation
     ⍝ This prevents TRANSLATION errors in classic interpreters
          (file key)←2↑(condEncl ra),0
          ⍝ For Classic do we transform special character like RANK into ⎕Uxxxx?
          :If transform←82=⎕DR''
              transform←{6 911::1 ⋄ '1'∊⎕SE.SALT.Settings'mapprimitives'}0
          :EndIf
     ⍝ Try to read using the V15 system fns, if it fails, revert to the old code:
          :Trap 92 2
              r←{⍵↓⍨-(⎕UCS 10)=¯1↑⍵}1⊃⎕NGET file(256×transform)
              →0
          :EndTrap
         
          clean←{⍵} ⍝ do we need to remove anything from the file?
          :If 2=⎕NC'remove'
              clean←~∘remove
          :EndIf
          r←'' ⋄ ⎕TRAP←19 'E' '→0' ⍝ ⎕MAP error if empty file
          nc←⍴r←256|83 ¯1 ⎕MAP file
          utf8←239 187 191≡3↑r ⋄ ucs2←(2 2⍴254 255 255)∧.=2↑r ⍝ 254 255=big endian
     ⍝ An updated SALT won't know about this new setting
          :If transform
              b←∨/ucs2 ⋄ from←Special ucs2 ⋄ to←(Uxxxx ucs2),¨⊂ucs2[1]⌽32,b/0
              nc←⍴r←r numReplace from to
          :EndIf
          :If utf8 ⍝ UTF-8 header
              r←'UTF-8'⎕UCS clean 3↓r
          :ElseIf ∨/ucs2
              r←⎕UCS clean 1↓(256*ucs2)+.×⍨(2,⍨nc÷2)⍴r
          :Else ⍝ assume UTF-8 and trap any error
              :Trap 0
                  r←'UTF-8'⎕UCS clean r ⍝ decode using KEY
              :Else
                  ⎕DMX.Message ⎕SIGNAL ⎕EN
              :EndTrap
          :EndIf
        ∇

        ∇ lu←LU
          :Access Shared Private
          lu←'abcdefghijklmnopqrstuvwxyzàáâãåèéêëòóôõöøùúûäæü' 'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÅÈÉÊËÒÓÔÕÖØÙÚÛÄÆÜ'
        ∇

        ∇ r←folder ListVersions name
          :Access Shared Private
          r←⎕IO⊃folder ListVersionsTS name
         ⍝ Version 0 equates the highest Vno if present
          r←r~(0<⌈/r)/0 ⍝ remove it if versioning in effect
        ∇

        ∇ (r ts)←folder ListVersionsTS name;ext;files;se;u;ok
          :Access Shared Private
     ⍝ Return version numbers and the timestamp of the associated files.
     ⍝ 'name' is a name always followed by an extension ONLY (no verno)
     ⍝ 0 as verno represents a file with no version #.
          (name r ext)←splitName lCase⍣WIN⌷name ⍝ lowercase for Windows
          se←-⍴ext←'.',ext,(0∊⍴ext)/1↓SALTEXT
          folder←folder,(FS=¯1↑FS,folder)↓FS
          r←⍬ ⍝ file names with special characters like '(' create problems under Unix
          :If 0<⍴⎕IO⊃(ts files)←2 4⊃¨⊂'a'Dir folder,name,'*',ext
             ⍝ There is a possibility that Dir reported too many names:
              ok←(1⊃¨splitName¨lCase⍣WIN¨files)∊⊂name
          :AndIf 0<⍴⎕IO⊃(files ts)←ok∘/¨files ts
              u←(r⍳r)=⍳⍴r←{(('.'∊⍵)>' '∊v)×+/2⊃⎕VFI v←2⊃'.'splitLast ⍵}¨se↓¨files
              (r ts)←u∘/¨r ts
          :EndIf
        ∇

        ∇ r←file MakeAPLScript class;eAS;html;i;m;script
     ⍝ Merge PageClass source with apls file contents
          html←(i←((⍴eAS)↑¨file)⍳eAS←⊂'</APLScript>')↓file ⍝ Extract the non-APLScript part
          script←1↓(i-1)↑file
          script←({'<'∊1↑⍵~' '}¨script)/script ⍝ statements beginning with <
          :If (⍴class)≥i←class⍳⊂'⍝--- APLS Code Begins ---'
              class←¯2↓i↓class
              m←class∧.=¨' '
              class←(+/∧\m)↓(-+/∧\⌽m)↓class
          :EndIf
          r←(eAS~¨'/'),script,class,eAS,html
        ∇

          MergeMD←{0∊⍴ra←⍵:⍺ ⋄ ∧/keep←~merge←⊃∊/(new old)←1⊃¨¨⍺ ⍵:⍵,⍺ ⍝ all new
              ra[2,⍨¨old⍳merge/new]+←0,¨0 1∘↓¨2⊃¨merge/⍺ ⋄ ra,keep/⍺
          }

        ∇ enc←NewEncrypt
         ⍝ Provide a new encryption instance
          :If 0=⎕NC'⎕se.Dyalog.Symmetric'
              'Dyalog'⎕SE.⎕NS''
              ⎕SE.SALT.Load'[SALT]/tools/special/symmetric -target=⎕se.Dyalog'
          :EndIf
          enc←⎕NEW ⎕SE.Dyalog.Symmetric'Rijndael'
        ∇

        ∇ rslt←{att}NtDirX path;attrs;FileTimeToLocalFileTime;FileTimeToSystemTime;FindClose;FindFirstFileA;FindNextFileA;GetLastError;handle;keep;mask;max;next;ok;⎕IO;⎕ML
     ⍝ Return NT directory information
          :Access Shared Private
          ⎕ML←1 ⋄ ⎕IO←0 ⋄ rslt←4⍴⊂'' ⋄ max←2*32
          keep←(⍳32)~30 29 ⍝ all but Hidden & System
          :If 0≠⎕NC'att'
              keep,←('HS'∊att)/30 29
          :EndIf
          mask←(⍳32)∊keep
          FindDefine
          handle next←FindFirstFile path
          →(0=handle)/0 ⍝ file not found
          rslt←,⊂next
          :While 1=0⊃ok next←FindNextFile handle
              rslt,←⊂next
          :EndWhile
          :If 0 18∨.≠ok next
              ('ntdir error:',⍕next)⎕SIGNAL 11
          :EndIf
          ok←FindClose handle
          rslt←1 0 0 1 1 0 1 0/↓⍉↑rslt         ⍝ bin the unwanted elements
          (0⊃rslt)←(attrs←(32⍴2)⊤0⊃rslt)[27;]  ⍝ Get attributes into bits, note dirs
          rslt←(mask∧.≥attrs)∘/¨rslt           ⍝ discard hidden/system unless wanted
          rslt[1]←Filetime_to_TS¨¨rslt[1]      ⍝ put times into ⎕ts format
          (2⊃rslt)←max⊥max|⍉↑2⊃rslt            ⍝ combine size elements
          rslt←(~(3⊃rslt)∊1 2⍴¨'.')∘⌿¨rslt     ⍝ remove . & ..
        ∇

        ∇ text PutUTF8File ra;file;key;tn;⎕IO;s;BOM;transform
     ⍝ Write Unicode text to new file on disk
          (file key)←2↑(condEncl ra),0
          ⍝ Shall we transform ⎕Uxxxx?
          transform←(82=⎕DR'')∧'1'∊⎕SE.SALT.Settings'mapprimitives'
          ⍝ Try the V15 fns first, if it fails revert to the previous code:
          :Trap 2
              {}text'UTF-8-BOM'⎕NPUT file(1+256×transform)
              →0
          :EndTrap
          :Trap 22
              0 ⎕NRESIZE tn←file ⎕NTIE ⎕IO←0
          :Else
              tn←file ⎕NCREATE 0
          :EndTrap
          s←⍴text       ⍝ do we need a BOM?
          BOM←239 187 191/⍨s≠⍴text←'UTF-8'⎕UCS text
          text←BOM,text ⍝ write unsigned UTF-8 header
          :If transform
              text←(uxxxx 0)s numReplace⍨text numReplace(Uxxxx 0)(s←Special 0)
          :EndIf
          text←⌊text-256×text>127
          text ⎕NAPPEND tn 83
          ⎕NUNTIE tn
        ∇

        RSI←{(⍵+1)⊃⎕RSI}

        ∇ RemFile file;t
          →(file∧.=' ')/0
          RemoveVersions({q,⍵,q←'"'⍴⍨</'" '∊⍵}file),' -all -noprompt'
          t←⊃{⍺,'.',⍵}/1 0 1/splitName file
          t{22:: ⋄ ⍺ ⎕NERASE ⍺ ⎕NTIE ⍵}0
        ∇

        ∇ {cd}←ResetUCMDcache arg;tell;force;ev;t;uf;fol;tie;set;list;st;cf;fe;doit;problem
          ⍝ Reset the UCMD cache. tell=0: quiet, <0: force reset
          ⍝ We now add the necessary elements to make this work (Parser and Utils should be there already)
          '⎕SE.Dyalog.SALT'⎕NS cd←''
          ⍝ 2014/4/8   a new procedure is established to reduce startup time
          fol←0/cf←getEnvir'UCMDCACHEFILE'
          →0/⍨(⊂cf)∊'skip' 'SKIP'
          :If WIN
              ev←' ',⍨'-64'{⍺/⍨∨/⍺∊⍵}1⊃t←'.'⎕WG'aplversion'
              fol←'C:\Users\',⎕AN,'\Documents\Dyalog APL',ev,({⍵/⍨2>+\'.'=⍵}2⊃t),((80=⎕DR'')/' Unicode'),' Files'
              fol←FS,⍨{0::⍵ ⋄ 2⊃4070⌶⍬}fol ⍝ is the folder already defined internally?
          :Else
              ⍝ Ensure folder is there
              :If 0∊⍴cf ⍝ create it only if the env var is NOT specified
                  :Trap 2
                      fol←'/.dyalog',⍨getEnvir'HOME'
                      3 ⎕MKDIR fol
                  :Else
                      fol←'/',⍨,⊃⎕SH'mkdir -p ~/.dyalog && echo ~/.dyalog'
                  :EndTrap
              :EndIf
          :EndIf
          force←arg<0 ⋄ tell←|arg
          set←{⎕ML←1 ⋄ 1↓¨(⍵∊PATHDEL)⊂⍵}'∘',CMDDIR←⎕SE.SALT.Settings'cmddir'
          ⍝ Running under root we won't have a HOME env var and no spice folder
          list←{⎕SE.SALT.List'"',⍵,'" -raw -full=2 -recursive'}¨set
          uf←cf,(0∊⍴cf)/fol,'UserCommand20.cache'
          fe←{22::0 ⋄ 1⊣⎕FUNTIE(1 3⍴0 ¯1)⎕FSTAC ⍵ ⎕FCREATE 0}uf  ⍝ create the file if not there
          doit←1 ⋄ problem←1 ⋄ tie←0
          :Trap 22 ⍝ the creation may have failed because the folder did not exist
              tie←uf ⎕FSTIE 0                         ⍝ share tie it
              {⍵<3:tie ⎕FAPPEND¨⍨⍳3-⍵}2⌷⎕FSIZE tie    ⍝ ensure 2 components
              ⍝ Compare what we have in the cache file to the UCMDs' folder structure
              :If ~force
                  doit←list≢⎕FREAD tie,1              ⍝ if they don't agree we'll rebuild the list
              :EndIf
              problem←0
          :EndTrap
          tell←WIN<tell>problem
          :If doit
              ⍝ Rebuild the cache/list
              ⍞←tell/CR,'Rebuilding user command cache... ' ⋄ st←⎕AI[2]
              (CMDDIR t)←1 GetUCMDList'*'
              ⍝ Update the file
              :If ~problem
                  t list ⎕FREPLACE¨tie,¨2 1
              :EndIf
              ⎕SE.Dyalog.SALT.List←t
              ⍞←tell/'done',((DEBUG>0)/' in ',(⍕⎕AI[2]-st),' msecs'),CR
          :ElseIf ~problem
              ⎕SE.Dyalog.SALT.List←⎕FREAD tie 2
          :ElseIf 0=⎕NC'⎕SE.Dyalog.SALT.List'
              ⎕SE.Dyalog.SALT.List←0 10⍴0
          :EndIf
          ⎕FUNTIE tie
         
          cd←CMDDIR ⍝ return this value to caller
          setAutocomplete ⎕SE.Dyalog.SALT.List
          :If problem
              ⍞←CR,⍨CR,'* Unable to rebuild user command cache',(~fe)/': the path of "',uf,'" does not exist'
          :EndIf
        ∇

        ∇ r←SALTsetFile ⍝ file used under Unix for SALT settings
          :If 0=⎕NC'SALTSettingsFile'
              SALTSettingsFile←r←(⎕IO⊃⎕SH'echo ~',⎕AN),'/.dyalog/SALT.settings'
          :Else  ⍝ grab cached value
              r←SALTSettingsFile
          :EndIf
        ∇

        ∇ SetDelta(ref name version globalname xml key);ccr;d;ll;lw;nr;prev;t;targ;ts;src
          :Access Shared Private
          lw←⍕lastWrTime name
          :If 3∊⍴ref ⍝ variable
              (targ ref src)←ref
              varData(targ ref)name version lw(calcCRC src)xml key
          :ElseIf 4∊⍴ref ⍝ function
              (targ ref src ts)←ref
              nr←,remTag src
              :If 0.2=1|targ.⎕NC⊂ref ⍝ Dfn?
                  ccr←calcCRC nr
                  :If 1∊⍴nr ⍝ one liners must be split to accomodate the tag
                      nr←¯1↓¨nr ⋄ ll←'}'
                  :Else
                      ll←¯1↑nr ⋄ nr←¯1↓nr
                  :EndIf
              :Else
                  ccr←calcCRC nr ⋄ ll←⍬
              :EndIf
              d←'§' ⋄ nr,←⊂'⍝∇⍣',d,name,d,(⍕version),d,lw,d,ccr,d,⍕key
              d←targ.⎕STOP ref ⋄ ts←targ.⎕AT ref ⋄ ccr←1+1 targ.(400⌶)ref
              {z←÷⍵}' '∊1↑0⍴targ.⎕FX nr,ll ⍝ refix fn with info
              ⍝ Restore state
              d targ.⎕STOP ref ⋄ {0:: ⋄ ts targ.(1159⌶)ref}ccr targ.(400⌶)ref
          :Else ⍝ must be a ref
              prev←¯1
              :If 2∊ref.⎕NC'SALT_Data.Version'
                  prev←ref.SALT_Data.Version
              :EndIf
              ref.SALT_Data←ref.⎕NS''
              ref.SALT_Data.PreviousVersion←prev
              ref.SALT_Data.SourceFile←name
              ref.SALT_Data.Version←version
              ref.SALT_Data.LastWriteTime←lw
              ref.SALT_Data.GlobalName←globalname
              ref.SALT_Data.CRC←calcCRC ⎕SRC ref
              ref.SALT_Data.EKey←key
          :EndIf
        ∇

        ∇ s←Special ucs2 ⍝ 0 1 (LE) or 1 0 (BE) or 0 0 (UTF8)
          :If 1∊ucs2 ⍝ UCS2 values
              s←ucs2[2]⌽¨35,¨100 56 96 120 ⍝ big endian?
          :Else      ⍝ UTF8 values
              s←(226 141 164)(226 140 184)(226 141 160)(226 141 184)
          :EndIf
        ∇

        ∇ {R}←{rgn}Spice msg;arg;c;cmd;com;cs;details;DMX;help;helpfor;i;mon;name;names;nargs;reset;rez;rules;spc;switches;syntax;t;stop;swd;CallUserCode;cuc;ASN;dot;List
         ⍝ Called by the UCMD program. This program can also be used directly with a string as argument.
          (rgn ASN)←2↑{6::1 ⋄ rgn}0  ⍝ Is the result used (1) or simply displayed (0) ? do we Allow Short Names?
          ASN←(modeBit 5)∨UCauto<ASN ⍝ In this version we disregard "Allow Short Names" if autocomplete is in effect
         
         ⍝ Put a stop before the call if requested
          cmd↓⍨←¯2×stop←×DEBUG×' -'≡¯2↑cmd←rtb msg
          :If 0<i←+/∧\'?'=cmd←{(']'=1↑⍵)↓⍵}rlb cmd
              cmd←cmd[⍳i],' ',rlb i↓cmd ⍝ ensure only one space
          :EndIf ⍝ accept ?cmd
          :If (i>1)∧i=¯1+⍴cmd           ⍝ ]?? ≡ ]help
          :OrIf 'help'≡lCase rtb cmd
              R←UCMDGeneralHELP ⋄ →0    ⍝ more than 1 "?" alone or 'help': general help
          :EndIf
          R←0
          (cmd arg)←cmd splitOn1st' ' ⋄ cmd←lCase cmd
          cmd←,(1+'help'≡cmd)⊃cmd'?'    ⍝ accept "help xxx" instead of "?xxx"
         
          ⍝ In 13.1 ⎕DMX is introduced and used in some ucmds (e.g. ]DMX). We take a copy here
          :Trap DMX←0        ⍝ this may fail if ⎕DMX contains unusual objects
              'DMX'⎕NS ⎕DMX
          :EndTrap
         
     ⍝ If the UCMDs' location has not been enabled it will fail unless we automatically boot Spice:
          :If ~reset←0≠⎕NC'⎕SE.Dyalog.SALT.List'
          :OrIf reset←'ureset'≡cmd      ⍝ allow 'ureset' to force reset
              CMDDIR←{0∊⎕NC'CMDDIR':⍵ ⋄ CMDDIR}0
              t←⎕SI∊⍨⊂'GetUCMDList'     ⍝ is this a recursive call thru ⎕FIX?
          :OrIf t<CMDDIR≢⎕SE.SALT.Settings'cmddir'  ⍝ has the Spice folder changed?
              (1+reset/stop↑⎕LC)⎕STOP 1⊃⎕SI
              ResetUCMDcache-reset      ⍝ force reset
              setAutocomplete List←⎕SE.Dyalog.SALT.List
              R←(⍕1↑⍴List),' commands reloaded'
              →reset⍴0
              R←0
          :EndIf
         
          List←⎕SE.Dyalog.SALT.List
         
          helpfor←{0∊⍴⍵:'No files found for "',⍺,'"'
              c←('Group' 'Name' 'Description',[0.5]¨'=')⍪{⍵[AZaz⍋⍕⍵[;1 2];]}⍵[;cGroup cLname cDesc]
              c[1+Where 2≡/c[;1];1]←⊂'' ⍝ blank out all but 1st group names
              '"]??" for general help, "]?CMD" for more specific info on command CMD',⍨CR,⍨,CR,⍨⍕c}
         
     ⍝ One line help
          :If help←'?'=1↑com←cmd
              ASN←1 ⍝ short names allowed for help since autocomplete does not work there
              →0/⍨0≢R←displayNames arg List
              details←¯1+'?'+.=com
              com←lCase 1⊃arg splitOn' ' ⍝ the 1st word after '?' is the command to get help on
              ⍝ Is this a group?
              :If ~0∊⍴i←{⍵/⍳⍴⍵}List[;cGroup]∊⊂com~'?*'
                  R←helpfor List[i;] ⋄ →0
              :EndIf
          :EndIf
         
     ⍝ We need to bring in the command if it is unknown and AUTOmatic search is ON
          :If ⍬≡i←findCmdPos com    ⍝ look for 1 name; if not found
          :AndIf ⍱/'*?'∊com         ⍝ and no filter used
          :AndIf 'auto'≡⎕SE.SALT.Settings'newcmd'
              ResetUCMDcache 0
              setAutocomplete List←⎕SE.Dyalog.SALT.List
              i←findCmdPos com ⍝ try again
          :EndIf
         
     ⍝ If the command is still not found and help is wanted we look into groups and similar names
          :If 1≠⍴i
              :If help∧0∊⍴i
                  t←{⍺,'.',⍵}/List[;cLname,⍨(dot←'.'∊com)/cGroup]
              :AndIf 0∊⍴i←t limRegexFind com ⍝ look into commands
              :AndIf ~dot ⍝ group name?
                  i←List[;cGroup]limRegexFind com       ⍝ look into groups
              :EndIf
         
              :If 0∊⍴i ⍝ not found; has it been renamed?
              :AndIf 0<⍴i←NewNames[;1]{b⍳⍳1=+/b←(⊂⍵)∊⍨(⍴⍵)↑¨⍺}com
                  R←BS,'* Command ',⊃{'"',⍺,'" has been renamed "',⍵,'"'}/,NewNames[i;] ⋄ →0
              :EndIf
         
              :If 1<t←⍴i
              :OrIf 1<t←⍴i←Where((⍴com)↑¨List[;cGroup])∊⊂com
                  R←BS,'* Invalid user command; to see a list of all valid user commands that start with "',com,'", type ',CR,'      ]?',com,'*'
                  →0
              :ElseIf 0∊t
                  R←BS,'* Invalid user command; to see a list of all user commands type',CR,'      ]?'
                  →0
              :EndIf
          :EndIf
         
          :If help ⍝ Special case '?'
              :If 1<⍴,i
                  R←helpfor List[,i;] ⋄ →0
              :EndIf
         
              :Trap DEBUG↓0
                 ⍝ If parsing rules are given display them here
                  syntax←''
                  :If ''≢rules←splitRule cParse⊃,List[i;]
                      (nargs switches)←rules
                      syntax←' Syntax: ',{''≡⍵:⍵ ⋄ a,←' argument',(1<n←2⊃⎕VFI a←⍵~sl←'sSlL')/'s'
                          l←¯2⌽((1+n=1)⊃'last' 'all'),' arguments merged) ('
                          a,'.',⍨↑,/(1,∨/2 2⍴sl∊⍵)/'' ' or less'l}nargs
                      syntax,←CR{(0∊⍴⍵)↓⍺,⍵}describeSwitches switches
                  :EndIf
                  c←cFullName⊃,List[i;] ⋄ cs←⎕NS''             ⍝ only used to define space in it
                  R←BS,spc←⎕SE.SALT.Load'"',c,'" -target=cs'   ⍝ grab the cmd space, define in cs, keep ref to it
                  →0/⍨326≠⎕DR spc
                  name←cName⊃,List[i;] ⋄ cs.Group←cGroup⊃,List[i;]
         
                  :Trap 0 ⍝ can we put a stop on line 1 of the help fn?
                      (⍳stop)spc.⎕STOP'Help'
                  :Else
                      (stop/HelpStop)⎕STOP ⎕IO⊃⎕SI
                  :EndTrap
         HelpStop: ⍝ Stop here when stop requested for Help with Classes. Now TRACE into the next lines.
                  :Trap 2  ⍝ try the detailed syntax
                      com←details spc.Help name
                  :Else    ⍝ OK, the "old" way then
                      com←spc.Help name
                  :EndTrap
                  ⍝ The user may have decided to return a VTV instead of a char string, or maybe a char matrix
                  name←(cGroup⊃,List[i;]),'.',name ⍝ include the group name
                  com←'Command "',name,'".',syntax,{(CR∧.=2↑⍵)↓CR,⍵}{(0∊⍴⍵)∨326≠⎕DR ⍵:,CR,⍵ ⋄ ↑,/CR,¨⍵}com
                  com,←(CR=¯1↑com)↓CR,CR,'Script location: "',c,'"',CR
                  R←com
              :Else
                  R←BS,'* Unable to produce Help for command "',name,'":',{('⍎'=1↑⍵)↓⍵}⎕IO⊃⎕DM
              :EndTrap
              R←⊃∘⎕SE.Dyalog.Utils.layoutText⍣(~rgn)⊢R
          :Else
              :Trap DEBUG↓mon←0
     ⍝ We need to know the calling space. If we were called by <UCMD> this is in 'THIS'
                  :If t←'⎕SE.UCMD'≡2⊃⎕XSI,0 ⍝ called by ⎕SE.UCMD?
                  :AndIf 9=⎕NC'⎕SE.THIS'
                      cs←⎕SE.THIS ⍝ use what we know
                  :Else
                      cs←RSI 1+t                          ⍝ could be a user call
                  :EndIf
                  'c'⎕NS'' ⍝ we need a NAMED ns to get a full pathname
     ⍝ If we are debugging we use the object in the ws instead if possible
                  :If DEBUG>0
                  :AndIf 9=cs.⎕NC t←(com←cObjName⊃,List[i;]),'.SALT_Data'
                  :AndIf List[⍬⍴i;cFullName]≡⍬⍴splitName(cs⍎t).SourceFile
                      spc←cs⍎com ⋄ ⎕←1⌽'"* Debugging ws object "',com
                      spc.##.THIS←cs ⍝ THIS must be defined for some cmds
                  :Else
                      R←BS,spc←0 c ⎕SE.SALT.Load t←'"',(cFullName⊃,List[i;]),SALTEXT,'"'
                      →0/⍨326≠⎕DR spc ⍝ return msg if it failed
                  :EndIf
                  cmd←,spc.List
                  :If 1<+/t←(,¨cmd.Name)∊⊂com←cName⊃,List[i;] ⍝ more than 1 cmd with same name?
                      t←t∧(lCase¨cmd.Group)∊⊂cGroup⊃,List[i;] ⍝ then use the group
                  :EndIf
                  :If ''≢rules←⌽splitRule cmd[t⍳1].Parse
     ⍝ We have to prepare the parser for the number of arguments
                      (2⊃rules){(0<⍴⍺)/⍵,⍺}←'nargs='
                      arg←(⎕NEW ⎕SE.Parser rules).Parse arg
                  :EndIf
         
                  :If mon←com≢'UMonitor'
                  :AndIf mon←(9.1=⎕NC⊂'spc')∧(MONITOR>0)∧0<⍴names←spc.⎕NL-4.1 3.1
                      (¯1+⍳999)∘spc.⎕MONITOR¨names ⋄ mon←¯1
                  :EndIf
                  c.THIS←cs   ⍝ ensure we have a reference to the calling environment
                  c.RIU←⍬⍴rgn ⍝ and let know if the result is used
                  c.SourceFile←cFullName⊃,List[i;]
                  c.Group←cGroup⊃,List[i;]
                  c.WIN←WIN
                  c.DMX←DMX
         
                  ⍝ Create a program to call the user's code
                  cuc←⎕FX 1 stop 1/'R←CallUserCode args' '⍝ Program stopped on request; use →⎕LC to resume' 'R←{DEBUG↓85::0 0⍴0⋄E←85⌶⋄x←''spc.Run ⍵''⋄{11::0 ⋄ 0 E ⍵}⍕1:0 E x⋄E x}args'
         
                  ⍝ Is a stop requested before executing the code?
                  :If stop
                      :Trap 0 ⍝ can we put a stop on line 1 of the user fn?
                          1 spc.⎕STOP'Run'
                      :Else   ⍝ no, must be a fn in a class, let's stop here instead
                          1 ⎕STOP cuc
                      :EndTrap
                  :EndIf
         
                  ⍝ The following statement does the actual call to the user code.
                  ⍝ It runs the <Run> function passing as arguments the command to run and
                  ⍝ either the text that followed the command on the ]command line OR
                  ⍝ a namespace containing the arguments and the switches.
                  ⍝ For example, the command line
                  ⍝   ]mycmd  arg1 a2 -swx  -sw2=abc
                  ⍝ would generate com←'mycmd' and arg←' arg1 a2 -swx  -sw2=abc'
                  ⍝  OR, if the parsing rules are in effect:
                  ⍝ arg←⎕NS '' ⋄ arg.Arguments←'arg1' 'a2' ⋄ arg.swx←1 ⋄ arg.sw2←'abc'
         
         
                  ⍝ Run the code, cover non result case.
                  ⍝ Note that this uses a special feature of Dyalog which may not be there in a future release.
                  ⍝ Should you choose to use it you should put it in a cover function in case it is decommissioned.
         
                  R←CallUserCode com arg
         
                  :If (0≠mon)∧com≢'UMonitor'
                      i←(0<+/∘,¨0 1∘↓¨i)/(spc.⎕CR¨names){⍺ ⍵}¨i←spc.⎕MONITOR¨names
                      ⍎name,'←i',(2=⎕NC name)/' MergeMD ',name←MONITORNAME ⍝ set or add data
                  :EndIf
              :Else
                  R←BS,'* Command Execution Failed: ',mon{⍺=1:'Cannot monitor classes' ⋄ ('⍎'=1↑⍵)↓⍵}⎕IO⊃⎕DM,⊂'WS FULL'
                  MONITOR∧←mon≠1 ⍝ turn it off to prevent further problems
              :EndTrap
          :EndIf
        ∇

        ∇ r←{THIS}UCMD Input;dcl;in;nma;rlb;ur;notQ
     ⍝ User Command Processor. The result may be assigned to a global variable.
     ⍝ Most of the implementation is in ⎕SE.SALTUtils.Spice; this function only implements
     ⍝ "assignment syntax", i.e. ]varname←ucmd   and   ]⎕←ucmd
         
          rlb←{(+/∧\' '=⍵)↓⍵} ⋄ →(⍴in←rlb Input)↓0
         
          :If 0=⎕NC'THIS' ⋄ THIS←⎕IO⊃⎕RSI ⋄ :EndIf     ⍝ record calling environment
         
     ⍝ ]var←Cmd accepted; see if it makes sense if present. No spaces allowed.
          notQ←'⎕←'≢nma←(~' '∊nma)/nma←(⌽∨\⌽<\('←'=in)>∨\in∊'''"')/in
          dcl←'*'∊4⍴⎕STACK                             ⍝ were we at Desk Calculator Level?
          :If notQ∧(1<⍴nma)>0 2 9∊⍨THIS.⎕NC ¯1↓nma     ⍝ is the assignment legal?
              ur←'* assignment syntax error' ⋄ ur ⎕SIGNAL dcl↓2 ⋄ ⎕←ur ⋄ →
          :EndIf
          →(⍴in←(⍴nma)↓in)↓0                           ⍝ ]v← is the same as ]
         
          ur←(dcl,⍨dcl≤notQ×⍴nma)⎕SE.SALTUtils.Spice in
          {0:: ⋄ ⍎'⍵⎕signal 100'},' '                  ⍝ reset ⎕DM
          :If (0 2∊⍨10|⎕DR ur)∧1∊⍴⍴ur
          :AndIf (2↑ur)≡(⎕UCS 8),'*'                   ⍝ error signature
              ur ⎕SIGNAL dcl↓911 ⋄ ⎕←1↓ur ⋄ →0         ⍝ signal error only if called by a program
          :EndIf
         
          →(1=⍴nma)⍴0                                  ⍝ ]←cmd    discard result
          :If 0∊⍴nma ⋄ r←ur                            ⍝ no '←'   return result by this fn
          :ElseIf ~notQ ⋄ {⎕←⌽rlb⌽⍵}¨↓⎕FMT ur          ⍝ ]⎕←      display line by line
          :Else ⋄ {THIS⍎nma,'⍵'}ur                     ⍝ ]xx←cmd  store result in xx in calling namespace
          :EndIf
        ∇

        ∇ r←UCMDGeneralHELP
          r←⊃,/CR,¨1↓¨2↓⎕NR'UCMDGeneralHELP'
     ⍝                          User Commands - General Help
     ⍝
     ⍝ Execute command XYZ:                            ]XYZ
     ⍝ Capture command XYZ's result in var:            ]var←XYZ
     ⍝ Display brief information on cmd or group XYZ:  ]?XYZ  or ]help XYZ
     ⍝ Display detailed information on command XYZ:    ]??XYZ
     ⍝
     ⍝ List all available command names:               ]?
     ⍝ Display a summarised list of all commands:      ]?+
     ⍝ Display this help message:                      ]??    or ]help
     ⍝ List all the commands in folder /A/B/C          ]?/A/B/C
     ⍝
     ⍝ This version has a few more experimental features:
     ⍝
     ⍝ ]settings accept 2 new tracking elements: new and compiled
     ⍝   new is supposed to allow automatic saving of new objects edited using )ED
     ⍝   You will need to have a proper ]settings workdir (not SALT folder) before setting it
     ⍝   compiled will track compiled code and recompile it after ]loading it when in effect
     ⍝
     ⍝ ]boxing can be made permanent by saving the session.
     ⍝   It can also create a button to flip ]boxing on and off
     ⍝
     ⍝ ]lastresult can set the last result produced so it can be recalled
     ⍝
     ⍝ ]snap accepts a new -clean modifier to remove all tags BEFORE snapping
        ∇

          UnixFileExists←{0::(,'0')≡⎕IO⊃⎕SH'test -f ',⍵,'; echo $?' ⍝ suggested by AS
              ⎕NEXISTS ⍵}

        ∇ u←up Ux ucs2;U   ⍝ their ⎕U format in Classic. Arg same as Special.
          U←('⎕','uU'[⎕IO+up],'23')∘,¨'64' '38' '60' '78'
          :If 1∊ucs2
              u←ucs2[2]{,⍺⌽⍉0 256⊤⎕UCS ⍵}¨U
          :Else
              u←'UTF-8'∘⎕UCS¨U ⍝ done this way to prevent translation when saving
          :EndIf
        ∇

        Uxxxx←1∘' u←up Ux ucs2;U   ⍝ their ⎕U format in Classic. Arg same as Special.' ' U←(''⎕'',''uU''[⎕IO+up],''23'')∘,¨''64'' ''38'' ''60'' ''78''' ' :If 1∊ucs2' '     u←ucs2[2]{,⍺⌽⍉0 256⊤⎕UCS ⍵}¨U' ' :Else' '     u←''UTF-8''∘⎕UCS¨U ⍝ done this way to prevent translation when saving' ' :EndIf'

        ∇ str←default VerifyNEstring str;ok
          ⍝ Verify that the string is not empty
          ok←{⍱/0 2∊10|⎕DR ⍵:0 ⋄ 1≢≡⍵:0 ⋄ ~0∊⍴⍵}str←default{⍵,(0∊⍴⍵)/⍺}str~' '
          'Name and Group must be a non empty character string'⎕SIGNAL ok↓11
          ⍝ This line needed for CC
        ∇

        Where←{⍵/⍳⍴⍵}

        ∇ r←{options}WidthFit text;blk;n;pw;sh;sp;⎕IO;⎕ML ⍝ fit a list ⎕PW wide
          :Access Shared Private
          ⍎(0=⎕NC'options')/'options←⍬' ⋄ (pw blk sp)←options,(⍴,options)↓⎕PW,4 1 ⋄ pw←pw-sp
          ⎕ML←⎕IO←1 ⋄ sh←⊃,/⍴¨,¨text
          sh←⊃,/⍴¨text←(blk×⌈blk÷⍨sh+sp)↑¨text ⍝ adjust each word's size
          r←0⍴⊂''
          :While 0<⍴text
              r←r,,/(n←+/pw≥+\sh)↑text ⋄ (text sh)←n↓¨text sh
          :EndWhile
          r←↑r
        ∇

        addExt←{⍺←SALTEXT ⋄ ∨/(⍵='.')∧⌽∧\~⌽⍵∊'/\':⍵ ⋄ ⍵,'.',('.'=1↑⍺)↓⍺}

        ∇ last←str afterLast chars
          :Access Shared Private
          last←(-⊥⍨~str∊chars)↑str ⍝ string after last char(s)
        ∇

        ∇ r←list byGroup names;grps;cut;⎕ML;i;width
          ⍝ Regroup Names (col 1) in list by Group (col 2)
          (grps names)←↓⍉list[AZaz⍋⍕list[;i];i←cGroup cLname]
          ⎕ML←1 ⋄ cut←1,2≢/grps ⋄ width←⎕PW-4+⌈/∊⍴¨grps
          r←0 ¯1↓⍕(uCase¨cut/grps),[1.1]width{0 ¯2↓0 2↓⍕⍪⍺ Fold ⍵}¨cut⊂{((b⍳1)×∨/b←'.'=⍵)↓⍵}¨names
        ∇

        ∇ crc←calcCRC vtv;DH;n;⎕IO;⎕ML
         ⍝ Compute a CRC from a list of text vectors
          n←⍬⍴⍴DH←⊃,/LU ⋄ ⎕IO←⎕ML←1
          crc←DH[1+n⊥⍣¯1⊢(⍳⍴crc)+.×crc←∊(⎕UCS¨vtv),¨13]
        ∇

        ∇ n←{delfiles}cleanWS tgt;lst;sons;nr;s;b;fte
         ⍝ This fn is used to remove all traces of linking starting at tgt
          delfiles←{6::0 ⋄ delfiles}0 ⍝ delete associated file(s)?
          :If n←9∊#.⎕NC'SALT_Var_Data'
              :If tgt≡#
                 ⍝ If # delete all entries
                  n←⍬⍴⍴fte←#.SALT_Var_Data.VD[;2] ⋄ tgt.⎕EX'SALT_Var_Data'
              :Else
                 ⍝ If not # and SALT_Var_Data exists remove only appropriate entries
                  nr←⍴s←'.',⍨⍕tgt ⋄ n←+/b←(nr↑¨#.SALT_Var_Data.VD[;1],¨'.')∊⊂s
                  fte←b/#.SALT_Var_Data.VD[;2]
                  #.SALT_Var_Data.VD←(~b)⌿#.SALT_Var_Data.VD
              :EndIf
              RemFile¨delfiles/fte
          :EndIf
          tgt.⎕EX'SALT_Data'
          →0↓⍨{0::1 ⋄ 0⊣⎕SRC ⍵}tgt ⍝ skip scripted nss
         
         ⍝ Refix all fns without their tags (keep the ⎕AT info)
          :If 0<⍴lst←tgt.⎕NL-3.1 3.2 4.1 4.2
              :If ∨/s←0=∊⍴∘⍴¨nr←{fnData⊂⍵}¨tgt.⎕NR¨lst
                  n+←+/tgt FixWOtag¨s/lst
                  RemFile¨delfiles/(s/nr).SourceFile
              :EndIf
          :EndIf
         
         ⍝ Recursively process any sub space
          :If 0<⍴lst←tgt.⎕NL-9.1 9.4 9.5
         ⍝ Only legitimate ones
          :AndIf ∨/lst←(DF tgt){⍺∘≡¨(⍴⍺)↑¨⍵}DF¨sons←tgt⍎¨lst
              :If 0<⍴sons←(lst/sons)~tgt
                  n+←+/cleanWS¨sons
              :EndIf
          :EndIf ⍝ all nss
        ∇

        ∇ r←condEncl arg ⍝ conditional enclose
          :Access Shared Private
          r←⊂⍣(326≠⎕DR,arg)⌷arg
        ∇

        ∇ desc←{stem}describeSwitches switches;w;t;s;n;sn;i;details;line;⎕ML
         ⍝ Describe the switches in layman's terms
          ⎕ML←1 ⋄ i←+/s←t=1↑t←rlb switches
          line←(i>0)/i{6::'Accepts modifier',(1<⍺)/'s' ⋄ stem}details←desc←''
          :For w :In {⍵[⍋↑⍵]}rtb¨s⊂t
              :If (⍴s←w)≥i←⌊/w⍳'=:∊'             ⍝ does this switch have any extra definition?
                  s[i]←'=' ⋄ n←⍴s←(i+</s⍳'[=')↑s ⍝ is the value optional?
                  :If n<⍴w                       ⍝ is there a validation to perform?
                      sn←1⌽'''''',1↓s~'[=]'      ⍝ the switch name
                      :If w[i]='='
                          details,←∊CR,¨⎕PW Fold' Modifier ',sn,' accepts only values ',↑{⍺,', ',⍵}/{1⌽'""',⍵~'''"'}¨{⎕ML←3 ⋄ b←≠\⍵∊'''"' ⋄ (b∨⍵≠' ')⊂⍵}n↓w
                      :ElseIf w[i]='∊'
                          details,←∊CR,¨⎕PW Fold' Modifier ',sn,' accepts values consisting only of characters in the set "',n↓w,'"'
                      :Else
                          details,←CR,'The default value of modifier ',sn,' is "',n↓w,'"'
                      :EndIf
                  :EndIf
              :EndIf
              :If ⎕PW<1+(⍴line)+⍴s      ⍝ exceeding ⎕PW?
                  desc,←CR,line ⋄ line←s ⍝ yes, start a new line
              :Else
                  line,←' ',s           ⍝ no, append to current line
              :EndIf
          :EndFor
          desc,←((0=⍴t)≥CR∊desc)↓CR,t←line,details
        ∇

        ∇ com←displayNames(arg list);c;cs;dn;i;L;ll;ord;t;pat;plus;mincase;empty
         ⍝ List all the cmds available. arg is ]?'s arg, list is the list of all UCMDs, ln is the lowercase version of names
          mincase←list[;cMinLen]{l←(⍴⍵)⌊|⍺ ⋄ (uCase l↑s),l↓s←⍵}¨⍣(2|MODE)⊢list[;cLname] ⍝ Adjust casing to show min required letters?
          com←0 ⍝ result if nothing to show
          plus←'+'∊(1↑arg),¯1↑arg ⋄ empty←0∊⍴arg←arg~plus/'+'
         ⍝ The list may be reduced using a limited regular expression and the display may be expanded with a +
          :If pat←(plus>empty)∨∨/'?*'∊arg
              (mincase list)←(⊂({⍺,'.',⍵}/list[;cLname,⍨('.'∊arg)/cGroup])limRegexFind arg)∘⌷¨mincase list
          :EndIf
         ⍝ We display a tabular list if a pattern (or nothing) was requested AND no + specified
          :If (pat∨empty)∧~plus
              →0/⍨0∊⍴mincase  ⍝ skip this if none found, the calling program can then try groups
              ll←CR,∊CR,¨(⎕PW-1)Fold'Type "]?+" for a summary or "]??" for general help or "]?CMD" for info on command CMD.'
              :If modeBit 3
              :OrIf 0∊+/PATHDEL∊⍨t←CMDDIR   ⍝ only 1 folder? must be SALT's
                  com←(modeBit 4)/CR,((0⍕1↑⍴list),' commands:'),CR
                  com←ll,⍨com,,CR,list showNames noDotNames⍣(~pat)⊢mincase
              :Else ⍝ multiple paths, show cmds for each - this is a bit more work:
                  dn←⍒∊L←⍴¨c←∪{⍵⊂⍨~⍵∊PATHDEL}t,'∘',BootPath'spice'
                  com←'' ⍝ we search by path length to cover the case \X,\X\Y
                  t←c[dn]∘{1⍳⍨⍺≡¨L[dn]↑¨⊂⍵}¨list[;cFullName] ⍝ INDEXERR if no matching folder for a cmd
                  :If ∨/t>⍴dn ⍝ then try switching file separator
                      c←{(FS,FS,⍵)[('/\',⍵)⍳⍵]}¨c
                      t←c[dn]∘{1⍳⍨⍺≡¨L[dn]↑¨⊂⍵}¨list[;cFullName]
                  :EndIf
                  :For i :In ⍳⍴c{⍺}ord←dn[t]
                      :If ∨/cs←i=ord
                          com,←∊CR,¨(⎕PW-1)Fold(0⍕+/cs),' commands in "',(i⊃c),'":',CR
                          com,←CR,⍨,CR,list showNames{⍵[AZaz⍋⊃⍵]}noDotNames⍣(~pat)⊢cs⌿mincase
                      :EndIf
                  :EndFor
                  com←com,ll
              :EndIf
          :ElseIf plus               ⍝ display 1 line summary per cmd
              com←arg helpfor list
          :ElseIf ~isRelPath arg     ⍝ is this a request for help on a path or a specific command?
              com←arg helpfor 2⊃GetUCMDList arg'*'
          :EndIf
        ∇

        ∇ {r}←a fileCopy w
          :Access Shared Private
          (a w)←{q,f,q←'"'/⍨WIN>'"'∊f←fixFsep ⍵}¨a w
          r←⎕SH((⎕IO+WIN)⊃'cp ' 'copy /y '),a,' ',w
        ∇

        ∇ {r}←fileErase w;q
          :Access Shared Private
          :Trap 0
              r←1 ⎕NDELETE fixFsep w
          :Else
              r←⎕SH((⎕IO+WIN)⊃'rm ' 'erase '),q,(fixFsep w),q←('"'∊w)↓'"'
          :EndTrap
        ∇

        ∇ pos←{exact}findCmdPos name;n;g;rl;glen;b
          exact←{6::⍵ ⋄ exact}~ASN ⍝ exact match?
          :If ~'.'∊name
              →0↑⍨exact∨⍴pos←Where List[;cLname]∊⊂name          ⍝ find an exact match
              pos←Where((List[;cMinLen]⌈⍴name)↑¨List[;cLname])∊⊂name ⍝ find all matches
          :Else
              (g n)←name splitOn1st'.' ⋄ glen←exact×⍴¨List[;cGroup]
              →0↓⍨⍴pos←Where b←((glen⌈⍴g)↑¨List[;cGroup])∊⊂g    ⍝ group found?
              rl←b⌿List[;cLname cMinLen]                        ⍝ reduced list
              →0↑⍨exact∨⍴pos←Where b\rl[;1]∊⊂n                  ⍝ find an exact match
              pos←Where b\((rl[;2]⌈⍴n)↑¨rl[;1])∊⊂n              ⍝ try on all names
          :EndIf
        ∇

        ∇ ml←ml findMinLen list;i;b;U;grp;nam;nu
         ⍝ Find the minimum length required to enter a command.
         ⍝ Start with the unique names
          U←∨/b/⍨1=+⌿b←nam∘.≡∪nam←list[;2] ⋄ i←1
          ml←ml-999×0=ml
          :Repeat
              b←({∨/U/⍨1=+⌿U←⍵∘.≡∪⍵}i↑¨U/nam)∨i=U/ml
              (b/U/ml)⌈←-i ⋄ i+←1
          :Until ∧/b
          ⍝ This line needed for CC
        ∇

        fixBRname←'\[[^][;∘]+\]'⎕R(,⊂'{specialName ⍵.Match}')

        ∇ s←{dropLastFS}fixFsep w;i;ws;cd;path;om;WSsurr;sl;fs
          :Access Shared Private
         ⍝ Validate and Replace / or \ by proper OS file delimiter
          fs←(1+∨/'://'⍷w)⊃FS'/'        ⍝ remains / on Win if web protocol
          i←i/⍳⍴i←'/\'∊⍨s←w ⋄ s[i]←fs   ⍝ fix Folder separator
          ⎕SIGNAL 22⍴⍨WIN∧':\'>.=1↓3↑s  ⍝ cannot be relative path if drive specified
          sl←⍴WSsurr←'[ws]'
          :If om←(1↑s)≡,'⍵'             ⍝ turn ⍵ or ⍵/ into current workspace path
          :OrIf WSsurr≡sl↑s
              i+←fs=1↑s↓⍨i←(sl,1)[1+om]
              s←('⍵',fs),i↓s
              :If 'CLEAR WS'≡ws←⎕WSID   ⍝ if in clear ws, use current directory
                  s[⎕IO]←'.'
              :ElseIf isRelPath ws      ⍝ if a ⎕WSID is a Windows relative path
                  s←'.',fs,i,(0∊⍴i←pathOf ws)↓fs,2↓s
              :Else
                  s←(pathOf ws),1↓s,(1=⍴s)/fs
              :EndIf
          :EndIf
          :If cd←(2↑s,fs)≡'.',fs        ⍝ turn ./ into same path as current directory
          :OrIf (3↑s,fs)≡'..',fs
              path←cd{⍺:⍵ ⋄ (-1+⊥⍨~⍵∊'/\')↓⍵}(-(¯1↑i)∊'/\')↓i←CD''
              s←path,(1+~cd)↓s,(1=⍴s)/fs
          :EndIf
          :If 0=⎕NC'dropLastFS'
          :OrIf dropLastFS
              s↓⍨←-fs=¯1↑s
          :EndIf
        ∇

        fixTabs←{{∧/b←⍵≠⎕UCS 9:⍵ ⋄ (b-4×~b)\b/⍵}¨⍵}

        ∇ space fixTs tags;⎕ML
         ⍝ Fix the timestamp/user name and compiled on the fns defined in the tags
         ⍝ Each tag describes the program name, the user name, the ts (⎕TS form) and if compiled
          →(⍴tags)↓0
          ⎕ML←1 ⋄ tags←0 1↓↑tags splitOn¨'!' ⋄ tags[;3]←num¨tags[;3] ⋄ tags[;2]~←' '
          :If ⎕SE.SALTUtils.SETTS
              :Trap 0
                  {6:: ⋄ DEBUG>0:⎕←⍵}tags[;1]space.(1159⌶)⍨0 1 0 1\tags[;3 2]
              :EndTrap
          :EndIf
          :If ⎕SE.SALTUtils.SETCOMPILED
              {}(1+(4↑[2]tags)[;4]∊⊂,'1')space.(400⌶)¨tags[;1]
          :EndIf
        ∇

        ∇ {(nam val xml)}←la fixVar src;drop;l1;⍙⍙⍙0;⎕ML ⍝ fix the source of a variable in the target space
         ⍝ Fix a variable. la is TARGET,  VALUE only, name, protected
          :If 0∊⍴nam←(3⊃la)~' '
              nam←{1↓¯1↓⍵[⍳⍵⍳'←']}⎕IO⊃1/src
          :EndIf
          ⎕ML←1 ⋄ drop←2+⍴nam ⋄ l1←2⌊⍴src
         ⍝ Find the format:
          :If xml←'<'∊1↑(drop×l1=1)↓l1⊃,src
              ⍙⍙⍙0←drop∘{⎕SE.Dyalog.Utils.fromXML ⍺↓∊⍵}
          :Else
         ⍝ Define a fn to recreate the variable
              l1←⎕FX(⊂nam,'←⍙⍙⍙0 S;_'),(1<⍴src)↓(1↑⍨⍴src)↓¨src ⍝ remove first line if more than one
              'Unable to recreate variable (too big)'⎕SIGNAL 911 if l1≢'⍙⍙⍙0'
          :EndIf
          :Trap 0
              :If ~la[2]
                  :If la[4]∧×la[1].⎕NC nam
                      val←'** "',nam,'" is already defined'
                  :Else
                      val←nam ⋄ nam(la[1].{⍎⍺,'←⍵'})⍙⍙⍙0 src
                  :EndIf
              :Else
                  val←⍙⍙⍙0 src
              :EndIf
          :Else
              val←'*** Unable to define variable',(1⌽'" "',nam),': ',⎕IO⊃⎕DM
          :EndTrap
        ∇

        ∇ r←fmtDate v
          :Access Shared Private
          r←0 19⍴'' ⋄ →0⍴⍨0∊⍴v
          r←v ⋄ →0⍴⍨isChar v ⍝ empty v is char: skip
          r←'I4,2(</>,ZI2),I3,2(<:>,ZI2)'⎕FMT⊃,6↑¨↓v
        ∇

        ∇ r←fmtVersion v;ab ⍝ arg can be a simple number, or a string
          :Access Shared Private
          ab←{'[',⍵,']'}  ⍝ neg numbers don't get decorators
          r←{3=10|⎕DR ⍵:ab⍣(0<⍬⍴⍵)⍕|⍵ ⋄ (0<⍴,⍵)/ab ⍵}v
        ∇

        ∇ ns←fnData arg;b;data;line;ln;lns;⎕ML
     ⍝ Fetch data in fn: sourcefile:version:lastwrtime:crc. arg is (source [name])
          ns←⍴⎕ML←1 ⋄ →0↓⍨ln←⍴lns←1⊃arg←2↑arg,0
          →0↓⍨1≡≡line←ln⊃lns
          →0↓⍨ln←ln-'}'=¯1↑line
          →0↓⍨∨/b←'⍝∇⍣'∘≡∘(3∘↑)∘rlb¨lns            ⍝ do we have such a line?
          →0↓⍨4≤⍴data←1↓¨('§'=line)⊂line←(b⍳1)⊃lns ⍝ is it kosher?
          data←5↑data,0 ⍝ previous versions did not have a key
          data[2]←2⊃⎕VFI 2⊃data ⋄ data[5]{⍺≡⍵:0 ⋄ ⍺}←⊂,'0'
          ns←⎕NS'' ⋄ ns.(SourceFile Version LastWriteTime CRC EKey Name)←data[⍳5],arg[2]
        ∇

        ∇ r←getEnvir w;t;⎕ML
          :Access Shared Private
         ⍝ SALT is a special env var that is assumed to be $DYALOG/SALT if not present
          w,←(WIN∧'HOME'≡w)/'path'
          :If (w≡'SALT')∧t←0=⍴r←2 ⎕NQ'.' 'GetEnvironment'w
         ⍝ We want SALT but it is NOT defined so let's build it
              r←r↓⍨-FS=¯1↑r←2 ⎕NQ'.' 'GetEnvironment' 'DYALOG'
              r←r,FS,'SALT' ⍝ default
          :ElseIf t∧w≡'USER' ⍝ USER is also special
              r←⎕AN
          :ElseIf WIN<t
          :AndIf UnixFileExists t←SALTsetFile
         ⍝ Read setting from the INI files
              r←GetUnicodeFile t
              (r t)←↓⍉↑' 'splitOn1st⍨¨r splitOn ⎕UCS 10⊣⎕ML←1
              r←rlb(r⍳⊂w)⊃t,⊂''
          :EndIf
        ∇

        getTs←{'⍝)('∘,¨,/'!',¨⍵,((⍕¨⍺.⎕AT ⍵)[;4 2]~¨'*'),⍕¨1 ⍺.(400⌶)⍵}

        ∇ r←isChar obj
          :Access Shared Private
          r←0 2∊⍨10|⎕DR,obj
        ∇

        ∇ r←isDir w
          :Access Shared Private
          r←1∊⎕IO⊃'a'Dir w
        ∇

        ∇ r←isFile w
          :Access Shared Private
          r←0∊⎕IO⊃'a'Dir w ⍝ assume max 1 result only
        ∇

        ∇ r←isHelp string
          :Access Shared Private
          r←(string~' ')≡,'?'
        ∇

        isReal←{(⍕⍺⍎⍵)≡(⍕⍺),'.',⍵}

        ∇ b←isRelPath path;s
          :Access Shared Private
         ⍝ A relative path is one not starting with / under Unix
         ⍝ or \ or [X:]\ under windows, taking into account URLs like http://
          →0↓⍨b←~1∊↑,/'://' ':\\'⍷¨⊂path
          b←~'/\'∊⍨(⎕IO+2×WIN∧':'∊s)⊃s←3↑path
        ∇

        ∇ s←lCase s;b;i;l;n;u;⎕IO
          :Access Shared Private
          :Trap 11
              s←819⌶s
          :Else
              n←⍴⎕IO⊃(l u)←LU
              →(∨/b←n>i←u⍳s)↓⎕IO←0
              (b/s)←l[b/i]
          :EndTrap
        ∇

        ∇ r←lastWrTime w ⍝ Last Write Time for file
          :Access Shared Private
          r←2⊃'a'Dir w
        ∇

        ∇ pos←list limRegexFind pattern;pat;from;to;t
         ⍝ Find elements in a list matching a limited regular expression
          :Trap 11
              from←'\.' '\*' '\?','\',¨t←'()[]{}' ⋄ to←'\\.' '.*' '.','\\'∘,¨t
              pat←'^','$',⍨from ⎕R to{⍵↓⍨'+'∊1↑⍵}pattern~' ' ⍝ turn into full regex
              pos←⎕IO+∪pat ⎕S 2⊢list
          :Else
              'Bad pattern'⎕SIGNAL 902
          :EndTrap
        ∇

        ∇ (dir path files)←name locateIn folders;dir;empty;files;nfs;path;t
     ⍝ Find the 1st occurence of file in list of folders
     ⍝ Return the folder it was found in the list provided, the full path and the files found
          :Access Shared Private
          empty←' '∧.=name ⍝ this will force return on the first dir in the list
          :For dir :In folders
              ⍝ If ClassFolder complains about the folder name we skip it
              :Trap 922
                  files←'a'Dir path←dir ClassFolder name⊣files←,⊂''
              :EndTrap
              :If empty∨0<⍴⎕IO⊃files
                  :If ∨/'?*'∊2⊃t←FS splitLast path
                      path←1⊃t
                  :EndIf
                  →0
              :EndIf
          :EndFor
          dir path files←'' '',⊂,¨4⍴⊂⍬ ⍝ default not found
        ∇

        ∇ {r}←makeDir path;D
          :Access Shared Private
         ⍝ Create a new directory, ignoring messages if any
          →(⍴r←fixFsep path)↓0
          :Trap 0
              r←3 ⎕MKDIR r
          :Else
              r←⎕SH'mkdir ',((~WIN)/'-p '),D,r,D←WIN/'"'
          :EndTrap
        ∇

        mergeTxt←{11::⍬ ⋄ (-⍴L)↓⊃,/(⊂⍣(1≡≡⍵)+⍵),¨⊂⎕UCS L←LINDEL}

        modeBit←{⍬⍴2(2*⍵)⊤MODE}

        ∇ r←msgBox arg;ans;Btns;Caption;choices;evt;hint;Mode;msgbox;N;No;parms;t;Text;Warn
         ⍝ Return 1, 2 or 3 for buttons pressed
          :Access Shared Private
          (Caption Text Mode Btns)←arg,(⍴arg)↓0 0 'Warn',('Yes' 'No')(⊂'OK')[Warn←1+arg[3⌊⍴arg]∊'Info' 'Msg']
          :Trap 0 ⍝ revert to ⍞ if GUI impossible
              {x←÷~3501⌶⍬}'is Ride connected?' ⍝ then no GUI
              'msgbox'⎕WC'msgBox'Caption Text Mode Btns('Event'(evt←61 62 63)1)
              r←4|evt⍳2⊃⎕DQ'msgbox'
          :Else
              (N No)←Warn⊃¨('nN' 'oO')('N (No)' 'O (OK)')
              choices←'yY',N,(t←3∊⍴Btns)/'cC' ⋄ hint←' Y/',1↓N,(t/'/C'),'? '
              ⎕←((⍴Text),⍳1≠≡,Text)⍴Text
              :While ~1∊ans←choices={⍞←⍵ ⋄ 1↑(⍴,⍵)↓⍞}⍕hint
                  ⎕←'** Please answer by Y (Yes) or ',No,t/' or C (Cancel)'
              :EndWhile
              r←(∨/3 2⍴ans)⍳1
          :EndTrap
        ∇

        noDotNames←{~modeBit 1:⍵ ⋄ ~∨/b←'.'∊¨⍵:⍵ ⋄ ~∨/b←b\m←m∨.∧1<+⌿m←s∘.≡∪s←{⍵↑⍨⍵⍳'.'}¨b/⍵:⍵ ⋄ (∪m/s),(~b)/⍵}

        ∇ r←num r        ⍝ only use ⎕VFI if character
          :Access Shared Private
          :If isChar r
              ((r='-')/r)←'¯' ⋄ r←2⊃⎕VFI r
          :EndIf
        ∇

        ∇ num←num numReplace fromto;st;⎕IO;∆;np;p;b;sf;i;from;to
         ⍝ fromto is the list of lists of numbers to replace
          ⎕IO←0
          :For from to :InEach fromto
              ∆←-/(st sf)←(⍴to),⍴from
              :If 0<np←⍬⍴⍴p←{⍵/⍳⍴⍵}from⍷num
                  :If ∆≤0
                      num[p∘.+⍳st]←np st⍴to ⋄ →∆↓0
                      b←(⍴num)⍴1 ⋄ b[p∘.+⍳sf]←np sf⍴sf↑st⍴1 ⋄ num←b/num
                  :Else
                      b←((⍴num)+np×∆)⍴1 ⋄ b[i←(p+∆×⍳np)∘.+⍳st]←np st⍴st↑sf⍴1 ⋄ num←b\num
                      num[i]←np st⍴to
                  :EndIf
              :EndIf
          :EndFor
        ∇

        ∇ r←pathOf filename
          :Access Shared Private
          :Trap 0
              r←¯1↓1⊃⎕NPARTS filename
              ((r∊'/\')/r)←FS
          :Else
              r←{(-1+⊥⍨~⍵∊'/\')↓⍵}filename
          :EndTrap
        ∇

        ∇ regClose HANDLE;RegCloseKey
          :Access Shared Private
          ⎕NA'U ADVAPI32.dll.C32|RegCloseKey U'
          {}RegCloseKey HANDLE
        ∇

        ∇ HANDLE←regGetHandle KEY;HKEY;KEY_ALL_ACCESS;rck;T
          :Access Shared Private
          HKEY←2147483649             ⍝ 'HKEY_CURRENT_USER' HEX 0x80000001
          KEY_ALL_ACCESS←983103       ⍝ HEX 0xF003F
     ⍝ The next line covers the case for reg & Unicode versions
          T←'A*'[⎕IO+80∊⎕DR'']
          'rck'⎕NA'I ADVAPI32.dll.C32|RegCreateKeyEx',T,' U <0T I <0T I I I >U >U'
          HANDLE←⊃2⊃rck HKEY KEY 0 '' 0 KEY_ALL_ACCESS 0 0 0
        ∇

        ∇ regPutString(SUBKEY STRING);HANDLE;Path;REG_SZ;set;T;uni
     ⍝ Stores the value of a Registry SUBKEY
          :Access Shared Private
          Path←(getEnvir'inifile'),'\SALT'
          HANDLE←regGetHandle Path
          STRING←,STRING
          REG_SZ←1 ⍝ String data type
          T←'A*'[uni←1+80∊⎕DR'']
          'set'⎕NA'I ADVAPI32.dll.C32|RegSetValueEx',T,' U <0T I I <0T I4'
          {}set HANDLE SUBKEY 0 REG_SZ STRING(uni×1+⊃⍴STRING)
          regClose HANDLE
        ∇

        ∇ value←regSetting arg;canbeempty;name;s;v
     ⍝ Return setting from the registry/environment
     ⍝ arg is: [1] Key, [2] default if not present or if empty, and [3] can it be empty
          arg←,condEncl arg
          (name value canbeempty)←arg,(⍴arg)↓'' '' 1
          :Access Shared Private
          :Trap 0
              :If canbeempty∨0<⍴v←getEnvir(WIN/'SALT\'),name
                  value←v
              :EndIf
          :EndTrap
        ∇

        ∇ {r}←remDir path
          :Access Shared Private
         ⍝ Remove a directory, ignoring messages if any
          →(⍴r←fixFsep path)↓0
          :If WIN
              r←⎕CMD'rd /Q /S "',r,'"'
          :Else
              r←⎕SH'rm -fr ',r
          :EndIf
         
        ∇

        ∇ name←remExt name;d ⍝ remove whatever extension after the last DOT (.)
          →0↓⍨∨/d←(name='.')∧⌽∧\~⌽name∊'/\'
          name↓⍨←-1+⊥⍨~d
        ∇

        ∇ nr←remTag nr;b
          ⍝ Remove any tag line in the fn
          →0↓⍨⍴nr/⍨←b←'⍝∇⍣'∘≢¨3∘↑∘rlb¨nr
          ⍝ Some versions have a trailing ' }', others a single '}'
          →0↓⍨(1 0 1≡b)∧(⊢/nr)∊' }'(,'}')
          →0/⍨'⍝'∊{⍵/⍨=\''''≠⍵}1⊃nr
          ⍝ Merge Dfn's last line with previous
          nr←,nr[1],¨'}'
        ∇

        remVerno←{(f v)←'.'splitLast ⍵ ⋄ ∧/(×⍴v),v∊⎕D:f ⋄ ⍵}

        ∇ r←{b}rlb w            ⍝ rem leading blanks
          :Access Shared Private
          :If 0=⎕NC'b'
              b←' '
          :EndIf
          r←(+/∧\w∊b)↓w
        ∇

        ∇ r←{b}rtb w            ⍝ rem trailing blanks
          :Access Shared Private
          :If 0=⎕NC'b'
              b←' '
          :EndIf
          r←(-⊥⍨w∊b)↓w
        ∇

        ∇ name saveSettings value;file;i;key;L;n;t;txt;v
     ⍝ Save name with value in registry
          :Access Shared Private
          key←3⊃SettingsTable[SettingsTable[;1]⍳⊂name;]
          :If WIN
              regPutString key value
          :Else ⍝ must be Unix world
              :Trap 22
                  txt←GetUnicodeFile file←SALTsetFile
         
                 ⍝ Read setting from the settings files
                  n←⍴txt←(txt splitOn L←⎕UCS 10)~⊂''
                  t←1⊃¨txt splitOn1st¨' '
                  :If n<i←t⍳⊂key
                      txt,←0 ⍝ key not there? append dummy
                  :EndIf
                  txt[i]←⊂key,' ',⍕value
                  txt←1↓⊃,/L,¨txt
              :Else
                  txt←key,' ',⍕value
              :EndTrap
         
              txt PutUTF8File file
              ⍝ Cover case where root is doing this
              :If 0∊1↑⎕AI
                  {}⎕SH'chown ',⎕AN,' ',file
              :EndIf
          :EndIf
        ∇

        ∇ setAutocomplete list;ev
          →V14↓0
          ev←(uCase¨list[;1]){⍺,'.',⍵}¨list[;2]
          :Trap 11 ⍝ just in case this Ibeam is not defined
              {}2350⌶']',¨ev,{⍵/⍨∨/m/⍨1=+⌿m←⍵∘.≡∪⍵}list[;2]
          :EndTrap
         
        ∇

        setFlag←{2 ⎕NQ'.' 'SetDFlags'⍵}

        showNames←{modeBit 2:⎕PW ¯2 ⎕SE.Dyalog.Utils.showCol{⍵[AZaz⍋⊃⍵]}⍵ ⋄ </modeBit¨1 3:⍺ byGroup ⍵ ⋄ ⎕PW 8 WidthFit{⍵[AZaz⍋⊃⍵]}⍵}

        ∇ path←specialName name;nwb;pfs;rp
         ⍝ Change any [name] into path
          :Access Shared Private
          →0↓⍨'['=1↑path←name
          path←getEnvir uCase nwb←((rp←name⍳']')↑name)~'[]' ⍝ uCase for Unix
          ('Folder cannot be resolved: ',name)⎕SIGNAL 922⍴⍨0∊⍴path
          rp+←FS∧.=(pfs←¯1↑path),1↑rp↓name
          :If 'dyalog'≡lCase nwb ⍝ 'Dyalog' needs the 'bin' folder removed
              path↓⍨←¯4×(FS,'bin')≡¯4↑(-pfs=FS)↓path
          :EndIf
          path,←rp↓name
        ∇

        ∇ r←a splitLast w;p ⍝ Split on last occurrence of a in w
          :Access Shared Private
         ⍝ Assume in first pos if not there
          p←-⌊/(⌽,w)⍳a ⋄ r←(p↓w)((1+p)↑w)
        ∇

        ∇ (stem ver ext)←{def}splitName name;d;n
         ⍝ Split filename into constituent parts
          ⍎(0=⎕NC'def')/'def←1' ⍝ do we assume a default?
          ext←def/1↓SALTEXT ⋄ ver←0⍴stem←name
          →0⍴⍨∧/d←(name≠'.')∨d≠¯1↑d←+\name∊'/\' ⍝ any dots?
          stem←name↓⍨n←-1+⍴ext←(-⊥⍨d)↑name ⋄ n←-⊥⍨n↓d
          →0↓⍨n{(⍺>-⍴⍵)∧(' '∊v)<(,1)≡⎕IO⊃⎕VFI v←⍺↑⍵}stem
          stem←stem↓⍨-1+⍴ver←n↑stem
        ∇

        ∇ strs←str splitOn char;⎕ML
          :Access Shared Private
          ⎕ML←1 ⍝ char may be multiple
          strs←1↓¨(strs∊char)⊂strs←(1↑char),str
        ∇

        ∇ r←str splitOn1st chars;p;⎕IO
         ⍝ Split on 1st occurrence of any chars in str
          :Access Shared Private
          ⎕IO←0 ⋄ p←⌊/(,str)⍳chars ⋄ r←(p↑str)((1+p)↓str)
        ∇

        splitOnNL←{(13 GetUnicodeFile ⍵)splitOn ⎕UCS 10}

        splitRule←{' '∧.=R←rlb ⍵:'' ⋄ (1↑⍵)∊⎕D:rlb¨R splitOn1st' ' ⋄ ''R}

        ∇ yes←string startsWith subs
          :Access Shared Private
          yes←subs≡(⍴subs)↑string
        ∇

        strIndex←{∨/b←⍺∊⍵:b⍳1 ⋄ (1=+/b)×1⍳⍨b←((⍴s)↑¨⍺)∊⊂s←,1⊃,⍵}

        ∇ name←name trimAt char;⎕IO
         ⍝ Truncates a character vector at the char delimiting byte.
          :Access Shared Private
          ⎕IO←0 ⋄ name↑⍨←name⍳char
        ∇

        ∇ s←uCase s;b;i;l;n;u;⎕IO
          :Access Shared Private
          :Trap 11
              s←1(819⌶)s
          :Else
              n←⍴⎕IO⊃(l u)←LU
              →(∨/b←n>i←l⍳s)↓⎕IO←0
              (b/s)←u[b/i]
          :EndTrap
        ∇

        uxxxx←0∘' u←up Ux ucs2;U   ⍝ their ⎕U format in Classic. Arg same as Special.' ' U←(''⎕'',''uU''[⎕IO+up],''23'')∘,¨''64'' ''38'' ''60'' ''78''' ' :If 1∊ucs2' '     u←ucs2[2]{,⍺⌽⍉0 256⊤⎕UCS ⍵}¨U' ' :Else' '     u←''UTF-8''∘⎕UCS¨U ⍝ done this way to prevent translation when saving' ' :EndIf'

        valWithRef←{16::1 ⋄ 1∊{9∊⎕NC'⍵':1 ⋄ ≡⍵}¨∊⍵}

        ∇ {ns}←{rem}varData arg;b;gd;gns;gv;NI;set
         ⍝ Fetch or Set data in ws for variable given as arg (1 or 7 elements)
          NI←7 ⋄ set←1<⍴,arg ⋄ ns←⍬ ⋄ gns←¯3↓gv←'#.SALT_Var_Data.VD'   ⍝ return ⍬ if not found
          :If 0=⎕NC gv                                                 ⍝ initialize if absent
              →set↓0
              ⍎gns,'←#.⎕ns⍬ ⋄',gv,⍕'←0 ',NI,'⍴0'
          :EndIf
          gd←⍎gv                                                       ⍝ fetch whole data
          :If 0<b←NI-2⊃⍴gd
              gd←0,⍨⍣b⊢gd                                              ⍝ account for pre V2.32 versions
          :EndIf
          :If 0=⎕NC'rem'
              rem←0
          :EndIf
          (1↑arg)←⊂{'['∊ns←DF 1⊃⍵:⍵ ⋄ ns,'.',2⊃⍵}1⊃,arg
          :If ~∨/b←gd[;1]∊⍬⍴arg
              →(rem≥set)/0
              b,←~gd⍪←0
          :EndIf
          ns←⎕NS'' ⋄ ns.Name←{326∊⎕DR ⍵:2⊃⍵ ⋄ ⍵↑⍨-⊥⍨'.'≠⍵}⎕IO⊃,arg
          ns.(Pathname SourceFile Version LastWriteTime CRC Format EKey)←,b⌿gd     ⍝ find actual values
          :If rem
              gd←(~b)⌿gd
          :EndIf
          :If set
              gd[b⍳1;]←arg   ⍝ NI elements
          :EndIf
          :If rem∨set
              ⍎gv,'←gd'                                     ⍝ reset globally
          :EndIf
        ∇

        ∇ r←name ∆VCR object;xml ⍝ create a visual char represention of a variable
          xml←0
          :If 2≡|≡name
              (name xml)←name
          :EndIf
          :If xml
              r←'⌷',name,'←',(⎕UCS LINDEL)⎕SE.Dyalog.Utils.toXML object
          :Else
              r←'⌷',name,'←',⎕SE.Dyalog.Utils.repObj object
          :EndIf
        ∇

        ∇ r←r ∆default value
     ⍝ Default CHARACTER STRING r BY value IF IT ≡0
          :Access Shared Private
          →(r≡1)⍴0 ⍝ also a valid value
          →(r≡0)↓mod
          r←value ⋄ →0
           ⍝ r IS not SCALAR 0 - ⍎ IF value IS NUMERIC
         mod:→(' '=1↑0⍴value)/0
          r←2⊃⎕VFI{b\⍵/⍨b←','≠⍵}r ⍝ accept , between values
        ∇

        ∇ parms←{patn}∆parse arg;narg;p;⍙
     ⍝ In this version we make use of the Parser in ⎕SE.
     ⍝ arg is argument, number of expected args (default any number)
          narg←''
          :If 2∊|≡arg
              (arg narg)←arg
          :EndIf
          narg←'upper prefix=⍙',(0<⍴narg)/' nargs=',narg←,⍕narg
          p←⎕NEW ⎕SE.Parser(patn narg)
          parms←p.Parse arg
          ⍙←1⌽')(',⍕'⍙'parms.⎕NL-2 ⍝ all ⍙ names
          ⍎⍙,'←parms.',⍙ ⍝ assign ⍙ names in current namespace
          parms←parms.Arguments
        ∇

        ∇ str←∆propagate switches;b;sw;v
     ⍝ This fn will recreate a string of the switches in order to be passed on another cmd
     ⍝ e.g. ⎕SE.SALT.Cmd myarg,∆propagate 'VERSION'
          str←''
          :If 1∊b←0≢¨v←⍎¨sw←'⍙',¨switches splitOn' '
              str←∊(b/v){' -',1↓⍵,(1≢⍺)/'=',⍕⍺}¨b/sw
          :EndIf
        ∇

    :EndNamespace
    :Class Parser
⍝ Parse a string and set modifiers values accordingly. DanB2008 Version 1.31
⍝ See Vector article in vol 26.1 for rules and logic behind.

⍝ This class has a few more features.
⍝ To use many of these features call the class with a second string containing
⍝ 0 or more of 'nargs=n  error=nnn  allownospace  upper  prefix=  modifiers='

⍝ You can specify
⍝ - the exact number of arguments or                                    (nargs=N)
⍝ - that the last argument includes all remaining text (LONG) and/or    (nargs=nL)
⍝ - the minimum and maximum number (SHORT) of arguments                 (nargs=n1-n2  or  nargs=mS)
⍝ - the lowest generated error can be specified                         (error=nnn)
⍝ - no space may be allowed before each modifier (/a/la/DOS)            (allownospace)
⍝ - modifier names can be UPPER or mixcased. UPPER means they can be entered in any case but used UPPER case. (upper)
⍝ - use a prefix for variable names holding the modifiers value (e.g. /0 can go into variable X0)  (prefix=X)
⍝ - use " instead of ' (the other quote can then be used in between as in "I'm")
⍝ - there is a program to provide default values
⍝ - there is a program to propagate the modifiers
⍝ - you can specify the minimum number of characters to enter for each modifier
⍝ - you can specify arguments using +_n=...
⍝ - you can perform some list or set validation or define a default value for a modifier
⍝ - you can specify where the modifiers must appear                     (modifiers=first|left|¯1, last|right|1, any|0)

⍝ Example: create a parser accepting 3 modifiers in UPPERCASE: DATE which takes a value, SW2 which does
⍝ not take a value and SW3 which MAY take one of 'a', 'bc' or 'def'
⍝   ps←⎕NEW Parser ('/date= /sw2 /sw3[=]a bc def'  'upper')    ⍝ modifiers' variables are uppercase
⍝   data←ps.Parse 'dsa x /sw3=bc /dat="07/08/28"'   ⍝ will return a namespace containing
⍝   data.SW2≡0                                      ⍝ 2 strings and set 3 modifiers
⍝   data.SW3≡'bc'
⍝   '07/08/28'≡data.Switch 'DATE'                   ⍝ get the modifier value from the table
⍝   '07/08/28'≡data.DATE                            ⍝ or directly from the NAME

⍝ If only a one time parsing is needed the following will do:
⍝ data←(⎕new Parser '+sw1 +sw2...').Parse string

⍝ Example: create a parser accepting up to 4 arguments, one modifier accepting vowels only and
⍝ accept -deletefiles whose minimum number of entered letters must be 'delete':
⍝   pv←⎕new Parser ('-letter∊aeiou -delete(files)' 'nargs=4S')
⍝   words←pv.Parse '"I can''t"  ''arg 2''  -let=aaaooo  -delete'  ⍝ only 2 args supplied, minimum 'delete' supplied.
⍝   7 6≡⊃,/⍴¨words.Arguments

⍝ Example: create a parser accepting 2 to 4 args, one modifier defaulting to 'Paris' and one modifier OK
⍝   where←⎕new Parser ('$city:Paris $OK' 'nargs=2-4')
⍝   who←where.Parse 'two arguments $OK' ⋄ 'Paris'≡who.city ⋄ 1≡who.OK

        ⎕io←0 ⋄ ⎕ml←1
        (LOWER UPPER)←'abcdefghijklmnopqrstuvwxyzàáâãåèéêëòóôõöøùúûäæü' 'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÅÈÉÊËÒÓÔÕÖØÙÚÛÄÆÜ'
        MAXARGS←15                  ⍝ maximum arguments reported individually
        upperCase←{~1∊b←(⍴LOWER)>i←LOWER⍳s←⍵:⍵ ⋄ (b/s)←UPPER[b/i] ⋄ s}
        xCut←{⍺←⍵=1↑⍵ ⋄ 1↓¨⍺⊂⍵}     ⍝ exclusive cut
        fixCase←{⍵}                 ⍝ assume no fix
        sqz←{(b⍲1⌽b←' '=s)/s←' ',⍵} ⍝ remove double spaces, add leading ' '
        if←/⍨

        :field ERROR0←700           ⍝ errors are raised starting at this number
        :field DELIMITER            ⍝ the character used to delimit modifiers
        :field FORCESPACE           ⍝ whether a space MUST precede a modifier
        :field NARGS←⍬              ⍝ how many arguments must be entered (⍬=any number)
        :field LS←,0                ⍝ does syntax support Long/Short scope?
        :field PREFIX←''            ⍝ modifiers' prefix
        :field MODPOS←0             ⍝ 1=right, ¯1=left, 0=any

        ∇ init arg;model;features;b;f;na;mem;cut;n
    ⍝ Initialize class
          :Access public
          :Implements constructor
          (model features)←2↑⊂⍣(1=≡arg)+arg
          DELIMITER←⍬⍴model,'+'     ⍝ 1st char of 'model' delimits modifiers.
          FORCESPACE←' '≠DELIMITER  ⍝ force only if NOT already used as delimiter
          :If ' '∨.≠features
              Ps←⎕NEW(⊃⊃⎕CLASS ⎕THIS)' nargs= upper allownospace error= prefix= modifiers=' ⍝ allowed modifiers for the parser itself
              Pset←Ps.Parse 1↓sqz features
         ⍝ Find how many arguments this parser accepts.
         ⍝ This should be a ≥0 number either preceded or followed by the letter L or S (for 'Long/Short' scope)
              :If 0<⍴NARGS←na←{(0≢⍵)/⍵}Pset.nargs  ⍝ unspecified?
                  f←∨/LS←∨/2 3⍴'llL-sS'∊na ⋄ NARGS←⊃⌽⎕VFI b\na/⍨b←~na∊'lLsS-'
                  na←'^(\d+[sSlL]{0,2}|\d+-\d+[lL]?)$'⎕S'&'⊢na
             ⍝ We reject if Short/Long and 0 args OR bad #s OR Short and min=max
                  'Invalid number of arguments'⎕SIGNAL 11 if(f∧0∊NARGS)∨(0∊⍴na)∨(LS[1]∧=/2↑NARGS)
                  NARGS←¯2↑{⍵[⍋⍵]}NARGS,NARGS×~LS[1]
             ⍝ We find where the modifiers must appear (default: anywhere)
              :EndIf
              MODPOS←0 ¯1 ¯1 0 1 1 ¯1 0 1[(0 'LEFT' 'FIRST' 'ANY' 'RIGHT' 'LAST' '¯1',,¨'01')⍳⊂upperCase Pset.modifiers]
              FORCESPACE←~Pset.allownospace
              fixCase←upperCase⍣Pset.upper                 ⍝ ↓ allow starting errors in range 100-990
              'ERROR # must be from 100 to 990'⎕SIGNAL 11 if 100 991=.≤ERROR0←700 Pset.Switch'error'
              'Prefix must be a valid name'⎕SIGNAL 11 if 0>⎕NC'x',⍨PREFIX←(b≢0)/⍕b←Pset.prefix
              ⎕EX'Ps' ⍝ no need anymore and we don't want the instance to carry it around
          :EndIf
         
    ⍝ Parse the model
          SwTable←0 3⍴0
          :If 0<Nswitches←+/cut←(≠\Quotes model)<model∊1↑model
              SwTable←↑{3↑(1,1↓<\⍵∊'[=∊:')⊂⍵}¨cut xCut model
              SwTable[;1]←{deQuote(-⊥⍨' '=⍵)↓⍵}¨SwTable[;1]    ⍝ the values
              SwTable[;0]←f←~∘'()'¨b←fixCase¨SwTable[;0]~¨' '  ⍝ the name
              SwTable[;2]←1⌈('('∊¨b)×b⍳¨'('                    ⍝ minimum length to enter
              'modifiers must be unique'⎕SIGNAL 11 if f≢∪f
              'modifiers must be valid identifiers'⎕SIGNAL 11 if ¯1∊⎕NC PREFIX∘,¨f
          :AndIf 1∊b←∨⌿mem←''∘⍴¨'∊' '[∊]'∘.⍷f←SwTable[;1] ⋄ (mem f)←b∘/¨mem f
              'A set must be provided with ∊'⎕SIGNAL 11 if 1∊(1 3+.×mem)=∊⍴¨f
          :EndIf
         
    ⍝ If a number of arguments was specified define some vars for it
          :If 0<n←MAXARGS⌊⊃⌽NARGS
              SwTable⍪←('_',¨⍕¨1+⍳n),'=',⍪n⍴2
          :EndIf
         
⍝ Create a namespace based on that model
⍝ (we could have created a similar class but we would have had to use ⎕NEW instead of ⎕NS ⎕OR)
          pData←{'#'∊⍕1⊃⎕RSI:#.⎕NS ⍵ ⋄ ⎕SE.⎕NS ⍵}''
          f←SwTable[;0]
          :If 0<⍴PREFIX
              f←((Nswitches>⍳⍴f)/¨⊂PREFIX),¨f ⍝ prepend prefix for modifiers only
          :EndIf
          f←f~'Switch' 'Propagate' ⍝ ensure fn names not hidden by modifiers
          ('Conflicting names:',⍕∪b/f)⎕SIGNAL 11 if 1∊b←(f⍳f)≠⍳⍴f
          ⍎(0<⍴f)/⍕'pData.(',f,')←0'
     ⍝ Any defaulted modifier value?
          :If ∨/b←0≠n←{d×(2×b)+d←':'=(b←'['=⊃⍵)⌷⍵,0}¨f←SwTable[;1]
              ⍎'pData.(',(⍕b/SwTable[;0]),')←na'⊣na←⊃⍣(1∊⍴na)⊢na←b/n↓¨f
          :EndIf
          pData.⎕FX ⎕NR'Switch'    ⍝ do not use a ref to avoid keeping a copy of this ns
          f←⎕VR'Propagate' ⋄ f[f⍳'$']←DELIMITER
          pData.⎕FX f              ⍝ mixed letter names minimize clashing with modifiers' names
          f←pData.(⎕IO ⎕ML)←0 1
        ∇

⍝ This function is used to return a modifier's value, possibly defaulted, e.g.
⍝ sw←123 Switch 'abc' ⍝ if 'abc' has not been set 123 is returned.
⍝ If /abc=789 was specified in the parsed string the value returned will be ,789, not '789'

        ∇ r←{def}Switch s;⎕IO;v
⍝ Return modifier's value
          ⎕IO←r←0 ⍝ invalid modifiers are considered not there
          :Trap 3
              r←1⊃SwD[SwD[;0]⍳⊂,s;]
          :EndTrap
          :If 0≠⎕NC'def'        ⍝ even undefined modifiers can be defaulted
              :If 0≡r ⋄ r←def   ⍝ use default if not set
              :ElseIf (1≢r)∧2|⎕DR,def ⍝ num is 11, x3, 645, 1287, 1289
                  r←1⊃v←⎕VFI r  ⍝ make numeric if default is also numeric
                  ('value must be numeric for ',s)⎕SIGNAL 11↓⍨∧/0⊃v ⍝ <if> unavailable
              :EndIf
          :EndIf
        ∇

        ∇ mask←Quotes str;Q;qm;tq;n;i
    ⍝ Find where text delimited by ' or " starts/ends
          qm←str∊'''"' ⋄ i←⍳n←⍴Q←qm/str ⍝ work on quotes only
          tq←<\(i∘.<i)∧Q∘.=Q            ⍝ matching trailing markers
          mask←n⍴tq⌹(i∘.=i)-0 ¯1↓0,tq   ⍝ all trailing markers
          mask←qm\mask∨¯1↓1,mask        ⍝ lead & trail markers
        ∇

        ∇ str←deQuote str;m;lq
    ⍝ Remove quotes and double quotes
          →0↓⍨∨/m←str∊'''"'            ⍝ which quote to use
          lq←m<1↓1,⍨m←str≠str[m⍳1]     ⍝ last quote
          str←(lq<m∨=\m)/str           ⍝ those to keep
        ∇

⍝ Parsing function.
⍝ Spaces are used to delimit arguments.
⍝ Quotes must be used to include spaces or delimiters in arguments.
⍝ Modifiers can exist with or without [possibly defaulted] value or be elided.
⍝ Modifiers not mentioned are refused.
⍝ Shorter names are accepted but '=' MUST be used to supply values.

        ∇ data←Parse arg;Er;s;t;table;parms;swit;swmat;m;val;vnc;set;req;pat;nov;q;∆;i;p;bad;twv;Q;minlen;np;b;args;argvalues;defined;nda;new;swpos;argpos;sw;rtb
    ⍝ PARSE modifiers and reset argument. Version 3.2
          :Access public
    ⍝ Account for "
          Er←ERROR0 ⋄ s←-FORCESPACE ⋄ arg←' ',⍕arg
          'unbalanced quotes'⎕SIGNAL Er if ¯1↑t←≠\Q←Quotes arg ⍝ QUOTED
          swpos←s⌽t<arg⍷⍨(s-1)↑DELIMITER ⍝ where all modifiers start
    ⍝ This is where the modifiers' position matters.
    ⍝ If they are all to the right then we consider the first DELIMITER marks the beginning of the modifiers.
          argpos←(¯1⌽t)<(' '=¯1⌽arg)>arg∊DELIMITER,' '  ⍝ this is where each non quoted string NOT starting with a DELIMITER starts
          argpos∧←DELIMITER≠' '                         ⍝ if space is used as delimiter there are no arguments
    ⍝ If they are all to the left we need to consider the first argument's location
          :If MODPOS=¯1 ⍝ this will be the 1st (non modifier) string preceded by a space
              'modifiers must ALL precede arguments'⎕SIGNAL Er+6 if swpos∨.∧∨\argpos
              :If (⍴argpos)>i←argpos⍳1
                  (swpos arg Q)←(i-1)⌽¨swpos arg Q      ⍝ send the modifiers to the end
              :EndIf
              args←~∨\swpos
          :ElseIf MODPOS=0 ⍝ if they can be anywhere:
              i←⊂⊂⍋≠\b\{⍵≠¯1↓0,⍵}b/swpos⊣b←swpos∨argpos ⍝ where the modifiers are
              (swpos arg Q)←i⌷¨swpos arg Q              ⍝ move them to the right
              args←~∨\swpos
          :Else ⍝ must be to the right
              'modifiers must ALL follow arguments'⎕SIGNAL Er+6 if argpos∨.∧t←∨\swpos
              args←~t
          :EndIf
          parms←args/arg ⍝ separate the arguments from the modifiers
          rtb←{(-⊥⍨' '=⍵)↓⍵}
          data←{'#'∊⍕⎕THIS:#.⎕NS ⍵ ⋄ ⎕SE.⎕NS ⍵}⎕OR'pData'
          table[;1]←{':'∊1↑(b←'['∊1↑⍵)↓⍵:(1+2×b)↓⍵ ⋄ 0}¨,0 1↓table←SwTable[;⍳2]  ⍝ default values
          swit←{s←rtb ⍵ ⋄ n←fixCase rtb s↑⍨e←s⍳'=' ⋄ e≠⍴s:n((1+e)↓s) ⋄ n 1}¨swpos xCut arg
          swmat←SwTable[;0] ⋄ minlen←SwTable[;2]
         
    ⍝ Process each modifier separately according to their position
          :While 0<⍴swit
              (sw val)←⊃swit
              :If 1≠+/b←swmat∊⊂sw ⍝ exact matches
              :AndIf 1≠t←+/b←((minlen⌈⍴sw)↑¨swmat)∊⊂sw
                  m←((~p)/((×t)⊃'unknown' 'ambiguous'),' modifier: "',sw,'"'),(p←0∊⍴sw)/'modifier names cannot be empty'
                  m ⎕SIGNAL Er+1
              :EndIf
              vnc←'['=⊃1⊃(sw p)←,b⌿SwTable[;⍳2] ⍝ '[=]' means 'value not compulsory'
        ⍝ 'pat' is 1 modifier group complete with assignment and allowed values if present
              m←∨/(set t)←'∊:'=⊃vnc↓pat←,p ⋄ pat[m/vnc]←'=' ⍝ is the right hand string a set?
              :If t ⋄ pat←(1+2×vnc)↑pat ⋄ :EndIf        ⍝ remove default value
        ⍝ We need to check if a value is needed or not allowed
              req←'='=⊃vnc↓pat ⋄ nov←(0∊⍴val)∨twv←1≡val ⍝ required ⋄ no value set ∨ there w/o value
              m←1⌽'>no value allowed for modifier <',sw
              m ⎕SIGNAL(req∨twv)↓Er+2
              m←1⌽'>value required for modifier <',sw
              m ⎕SIGNAL(nov∧req>vnc)⍴Er+3
              table[i←b⍳1;1]←⊂val←deQuote val ⍝ remove extra quotes
        ⍝ Valid strings are supplied with the modifier: verify them if a value supplied
              :If twv<0<⍴p←(1+vnc+pat⍳'=')↓pat  ⍝ an arg?
            ⍝ Spaces are invalid in args
              :AndIf (set∧∧/val∊p)⍱set<(' '∊val)<∨/(' ',val,' ')⍷' ',p,' '
                  m←'invalid value for modifier <',sw,'> (must be ',(set⊃'ONE of' 'ALL in'),' "',p,'")'
                  m ⎕SIGNAL Er+4
              :EndIf
        ⍝ Assign in instance (no need to assign the _n vars, this will be done below)
              :Trap 2 ⋄ ⍎(i<Nswitches)/'data.',PREFIX,sw,'←val' ⋄ :EndTrap
              swit←1↓swit
          :EndWhile
         
    ⍝ Find how may arguments remain to be split depending on how many have been defined thru +_n=
          t←(¯1↑NARGS)-nda←+/defined←0≢¨argvalues←,Nswitches 1↓table ⍝ args left to set
          np←nda+⍴args←splitParms parms Q,⌊/LS[0]/t ⍝ the total # of arg entered
         
    ⍝ Make sure the number of arguments matches the required number
          :If 0<⍴NARGS ⍝ were there limits?
          :AndIf ∨/m←¯1 1=×np-NARGS ⋄ t←'too ',(m[0]⊃'many' 'few'),' arguments'
              t ⎕SIGNAL Er+5 if m∨.>0,LS[0]             ⍝ reject if too few or too many and not Long
          :EndIf
    ⍝ Was a number of arguments specified?
          :If 0<m←¯1↑NARGS ⍝ insert place holder for parameters specified as +_n=
              ((⍴args)↑(b←~defined)/argvalues)←args             ⍝ insert args at the right place
              data.{⍎⍕('_',¨⍕¨1+⍳⍴⍵)'←',(1<⍴⍵)↓'⊃⍵'}argvalues   ⍝ create the variables in the ns
              (,Nswitches 1↓table)←argvalues
              s←⍴t←(defined/argvalues),1↓(1+m-nda)↑(⊂⍬),args    ⍝ important: ensure prototype is ⊂⍬, not ⊂''
              args←{⍵↓⍨-⊥⍨⍵≡¨⊂⍬}t[⍋⍒s↑defined]
          :EndIf
          data.SwD←table
          data.Arguments←args
        ∇

        ∇ parms←splitParms(parms Q n);q;s;t;p;np;qq;q1;txt;bl;d
    ⍝ Find each token in the argument and use the delimiter to separate them.
    ⍝ The following allows us to deal with '' properly:
          bl←parms∊' ' ⋄ txt←≠\q←(⍴parms)↑Q ⋄ qq←txt<q∧1⌽q ⋄ q1←q>¯1⌽txt∨q ⍝ qq=double quote, q1=1st quote
          d←q1∨bl>txt ⍝ DELIMITERS: spaces NOT in quotes OR 1st quote not in text
          np←+/s←d>1↓d,0 ⍝ the start of each section
          p←d∧⌽∨\⌽s\n>⍳np
    ⍝ COMPRESSOR: quotes but (double quote or quotes used as delim) OR double delim.
          t←~(q>qq∨p)∨(p∧1⌽p)∨⌽∧\⌽bl ⍝ and trailing spaces
          q←~p←t/p
          parms←1↓¨p⊂q\q/t/parms ⍝ turn quote delimiters into spaces for Long below
        ∇

        ∇ str←{Del}Propagate modifiers;sw;b;v;si
⍝ Recreate a string of the modifiers in order to be passed to another command
⍝ e.g. myArgs.Propagate 'VERSION'
⍝ Invalid modifiers are ignored.
          str←'' ⋄ si←SwD[;0]∊sw←({⍺←⍵=1↑⍵ ⋄ 1↓¨⍺⊂⍵}' ',modifiers)~⊂''
          :If 1∊b←0≢¨v←si/SwD[;1]
              :If 0=⎕NC'Del' ⋄ Del←'$' ⋄ :EndIf
              str←∊(b/v){' ',Del,⍵,(1≢⍺)/'=',{∨/⍵∊Del,' ''"':Q,((1+⍵=Q)/⍵),Q←'"' ⋄ ⍵}⍕⍺}¨b/si/SwD[;0]
          :EndIf
        ∇

    :EndClass ⍝ Parser  $Revision: 27253 $

    :Class SALT
⍝ Simple APL Library Toolkit for Dyalog
⍝ 2016 02 09 DanB: Load now treats ⍝∇:require and ⍝!:require almost the same way
⍝                  Snap allows comma between -class values
⍝ 2016 04 04 DanB: Load now handles ⍝!require file:// in V14
⍝ 2016 05 02 DanB: changed some Load error messages
⍝ 2016 05 11 DanB: changed error msg in Boot
⍝ 2016 05 18 DanB: Snap keeps casing for file names
⍝ 2016 06 06 DanB: added -clean for Snap and reordered Loads automatically
⍝ 2016 06 14 DanB: Settings to allow empty workdir

        :Field readonly Public Shared Version←2.63

        :Include SALTUtils

        ⎕USING←0⍴⊂⍬ ⋄ ⎕IO←1 ⋄ ⎕ML←2 ⋄ ⎕WX←3

⍝ --- Private fields

        :Field Private  shared  _TRAP←(709+⍳5) 'C' '(⎕io⊃⎕dm)⎕signal⎕en'
        :Field Private  shared  CR←⎕av[4]
        APLV←'.'⎕wg 'aplversion'
        :Field Private  shared  WIN←(⊂,'W')∊APLV[3]
    APLV←{⍎(2>+\'.'=⍵)/⍵}2⊃APLV
        :Field Public   shared  FS←'/\'[1+WIN]
        :Field Public   shared  PATHDEL←'∘',':;'[1+WIN] ⍝ delimiter accepted on input for SALT/UCMD paths
        :Field Private  shared  LINDEL←(~WIN)↓13 10
        :Field readOnly shared Public SALTEXT←'.dyalog'
        :Field readOnly shared Public SALTFOLDER←'[SALT]'
        :Field Public   shared FTS
     FTS←{0::0 ⋄ 1⊣0 (7↑6↑⎕ts) 0 'xx' (1159⌶)⍵} ⎕fx'r←CRef' 'r←2⊃⎕RSI'
     Pi←{⍵:0 ⋄ 3::0 ⋄ 'arm'≡3↑1⊃⎕sh'uname -m'}WIN

        split←{1↓¨(s∊⍵)⊂s←(⍵∊1↑⍺)↓⍵,⍺}

        :field shared SettingsTable←0 5⍴'' ⍝ name; description; registry name; default; value
        SettingsTable⍪←'compare;the comparison program to use;CompareCMD;APL;'split';'  ⍝ e.g. [ProgramFiles]BeyondCompare
   ⍝ Cmd Folders are locations where Spice commands are stored - their existence is not challenged
   ⍝ 2nd folder is something like this: C:\Users\DanB2\Documents\Dyalog APL 14.0 Unicode Files
        UserFolder←'[HOME]',(WIN/FS,'Documents'),FS,'MyUCMDs'
        SettingsTable⍪←('cmddir⍟the list of Spice folders (commands) to use separated by ∘⍟CommandFolder⍟',SALTFOLDER,FS,'spice∘',UserFolder,'⍟')split'⍟'
        SettingsTable⍪←'debug;debug level;DebugLevel;0;0' split';'
        SettingsTable⍪←'editor;the editor program to use;EditorCMD;notepad;'split';'
        SettingsTable⍪←'edprompt;editor confirmation prompt;EdPrompt;1;' split';'
        SettingsTable⍪←'mapprimitives;map some primitives to ⎕Uxxxx for Classic users;MapPrim;1;' split';'
        SettingsTable⍪←('newcmd;new command detection;CmdDetect;',((1+Pi)⊃'auto' 'manual'),';') split';'          ⍝ automatic for all but PI
        SettingsTable⍪←'track;element tracking;Track;;' split';'
        SettingsTable⍪←'varfmt;variables saving format;VarFmt;xml;' split';'            ⍝ only APL and XML so far
   ⍝ WorkFolders are locations where files are searched - their existence is not challenged
        SettingsTable⍪←('workdir⍟the list of storage folders to use separated by ∘⍟SourceFolder⍟',SALTFOLDER,'⍟')split'⍟'

⍝ --- Utilities

        if←/⍨                 ⍝ as in '→0 if condition'
        dotVer←{(0<⍵)/'.',⍕⍵} ⍝ >0 versions appear with a dot

⍝ --- Public methods

        ∇ Reboot
    ⍝ Bootstrap loader for SALT if the AddSALT Registry Parameter is 1 or Y or unset
          :Access public shared
          ⎕SE.SALTUtils.BootSALT
        ∇

        ∇ ref←Boot file;arg;at;cmd;data;f;folder;hasarg;i;line;method;name;nf;sfile;TARGET;tmp;txt;wf;⍙FUNCTION;⍙XLOAD;isFn;define;b;fd;quote
          :Access Shared Public
         
          :If isHelp file
              ref←'Boot filename [arg]' ''
              ref,←'Boot from a file containing instructions or a function' '' 'Modifiers:'
              ref,←⊂'-Xload      do not run the ⎕LX'
              ref,←⊂'If arg is a .dyapp file each line may be an instruction. If .dyalog it is a function to be executed'
              ref←⊃ref
              →0
          :EndIf
          :If hasarg←326∊⎕DR arg←file
              (file arg)←file
          :EndIf ⍝ maybe file followed by arg to fn to run
          file←fixFsep 1⊃1↑'-xload'∆parse file'1L'
          ⍙FUNCTION←⍙XLOAD∨SALTEXT≡(-⍴SALTEXT)↑file ⍝ maybe we can tell by the extension
         
          txt←i←0 ⋄ nf←⍴wf←getSetting'workdir'
          :While nf≥i←i+1
              sfile←(i⊃wf)ClassFolder file
              folder←(1⊃FS splitLast sfile),FS
              sfile←sfile{⍺,(⍵≢(-⍴,⍵)↑⍺)/⍵}(1+⍙FUNCTION)⊃'.dyapp'SALTEXT ⍝ add extension if absent
              :Trap 0
                  txt←splitOnNL sfile ⍝ read file as list of strings
              :EndTrap
          :Until txt≢0  ⍝ if we found the file, exit the for loop
          (⎕EM ⎕EN)⎕SIGNAL ⎕EN if txt≡0
         
          define←{(83 11∧.≠⎕DR n),⊂n←tmp.⎕FX{~⍙XLOAD:⍵ ⋄ ⍵/⍨~1∊¨'⍎⎕LX'∘⍷¨⍵}⍵}
         
     ⍝ Maybe this is a fn after all, let's check
          isFn←0 ⋄ 'tmp'⎕NS''
          :If ⍱/'load ' 'target '∊⍨5 7↑¨⊂lCase rlb ⎕IO⊃txt ⍝ must NOT be named 'load' or 'target'
              ⍙FUNCTION∨←⎕IO⊃(isFn name)←define txt
          :EndIf
          :If ⍙FUNCTION ⍝ we have a fn
              :If ~isFn ⍝ have brought it in yet?
                  (isFn name)←define txt
              :EndIf
              'Unable to define function in file'⎕SIGNAL 11 if~isFn
              at←,tmp.⎕AT name ⍝ find its valence
              tmp.a←(1+hasarg)⊃⍙XLOAD arg ⍝ if the fn needs an arg we supply the one given to Boot
        ⍝ Find what type of fn we are dealing with
              i←{2::1 ⋄ 85⌶⍵}'0'
              {}i{85::0 ⋄ ⎕TRAP←0⍴⊂'' ⋄ ⍺:1 tmp.(85⌶)⍵ ⋄ tmp.(85⌶)⍵}name,(0<1 2⊃at)/' a' ⍝ trap missing result
          :Else
              TARGET←'#'
              :For line :In txt                   ⍝ read each command
                  (cmd data)←line splitOn1st' '
                  data←rlb data
              ⍝ Empty line or comment skipped
                  :If ~∨/(1 2↑¨⊂cmd)∊('//')(,' ')(,'⍝')
                      :Select lCase cmd
                      :Case 'load'
                ⍝ Load's data is a file and possibly some switches
                ⍝ Let's see if the user thought of adding "s around the filename:
                          fd←∨/'''"'∊1↑data  ⍝ has the user already thought of it?
                          quote←¯1↑'"',fd⍴data
                          b←fd∧≠\data=1↑data ⍝ mask out filename
                          f←~b←∨\b<' -'⍷data ⍝ find switches
                          file←((~FS∊data)/folder),(-fd)↓fd↓rtb f/data ⍝ relative path?
                          file←quote,(fixFsep file),quote
                          :Trap 0
                              'Loaded: ',⍕Load file,' -target=',TARGET,b/data
                          :Else
                              ⎕←'** Load failed: ',⎕DMX.EM ⋄ →0
                          :EndTrap
                      :Case 'run'
                          TARGET⍎data
                      :Case 'target'
                          TARGET←data
                      :Else
                          ⎕←'Invalid command, ignored: ',line
                      :EndSelect
                  :EndIf
              :EndFor
          :EndIf
     ⍝ End Boot
        ∇

        ∇ ref←New class;args;cmd;hasargs;⍙VERSION;⎕TRAP
          :Access Shared Public
         
          :If isHelp class
              ref←'New classname [constructor arguments]' ''
              ref,←'Create an instance of class without naming it in the workspace' ''
              ref,←⊂'Modifiers:'
              ref←⊃ref,⊂'-version=       Select specific version'
              →0
          :EndIf
         
          :If hasargs←args←~isChar class
              (class args)←class
          :EndIf
         
          ⎕TRAP←_TRAP
          class←⎕IO⊃'-version='∆parse class'1'
          cmd←'"',class,'" -noname',(0≢⍙VERSION)/' -version=',⍙VERSION
          ref←#.⎕NEW(Load cmd),hasargs/⊂args
        ∇

        ∇ {ref}←{la}Load fname;allpaths;c;cname;ext;file;files;fix;fname;folder;found;i;info;isns;isvar;lastv;line;name;needed;nf;nosrc;nref;ns;path;protect;root;src;t;target;ts;v;ver;w;xml;⍙DISPERSE;⍙NOLINK;⍙NONAME;⍙SOURCE;⍙TARGET;⍙VERSION;⎕IO;⎕ML;⎕PW;⎕TRAP;dir;nc;okifempty;sw;cl;QV;keep;lines;req;msg;isrel
          :Access Shared Public
⍝ This function returns either a ref/name after defining the object in the ws or a ref/⎕OR (if -NONAME) of the Loaded object
⍝ or a VTV if the source was requested or several fns were defined or a message for anything else (failure preceded by ***)
          :If isHelp fname
              ref←'Load [path]scriptfile' '' 'Load object(s) (space/var/pgm) in the workspace'
              ref,←'' 'Modifiers:'
              ref,←⊂'-target=Namespace      Specifies target namespace for load'
              ref,←⊂'-disperse[=nam1,nam2]  Disperse elements in the target namespace specified'
              ref,←⊂'-noname                Only return value, do not create name'
              ref,←⊂'-nolink                Do not "link" loaded class to source file'
              ref,←⊂'-protect               Do NOT load if the name is already defined'
              ref,←⊂'-version=              Load specific version'
              ref,←⊂'-source[=no]           Return the source ("no" means fix but use no source)'
              ref←⊃ref ⋄ →0
          :EndIf
         
          ⎕TRAP←_TRAP ⋄ ⎕ML←2 ⋄ ⎕IO←1
          allpaths←,⊂fname←fixFsep 1⊃'-target= -noname -nolink -protect -source[=]no NO -disperse[=] -version='∆parse fname'1L'
          :If ('['=1↑fname)∨isrel←isRelPath fname
              allpaths←∪ClassFolder∘fname¨(getSetting'workdir')
          :EndIf
          lastv←0≡⍙VERSION ⍝ is the last version desired?
          ⍙VERSION←⌊num ⍙VERSION
          'la'Default 0 ⋄ (protect target okifempty)←3↑la   ⍝ target can be specified as a ref
          protect∨←⍙PROTECT                                 ⍝ protection can be specified 2 ways
         
    ⍝ Where to define the object
          :If (1≢⍙SOURCE)∧9≠⎕NC'target'
              target←CRef ⍝ this is where we were called from
          :AndIf 0≢⍙TARGET
    ⍝ If -NONAME is specified we may still have to bring in other nss if :require is found and
    ⍝ we define these where this code was called from (where else?)
              root←∨/(⊂lCase ⍙TARGET)∊'⎕se'(,'#')
              msg←((⍕DF target),'.',⍙TARGET),' is an invalid Target namespace'
              msg ⎕SIGNAL 911 if root<9≠target.⎕NC ⍙TARGET
              target←target⍎⍙TARGET
          :EndIf
         
          ref←⍬
    ⍝ Patterns are supported.
    ⍝ They only apply to objects and their filenames.
    ⍝ Folders used to store objects are not subject to filtering but their contents is.
    ⍝ A folder containing a single object (a namespace) is subject to filtering.
    ⍝ Such a folder will contain a file named 'name.txt'
    ⍝ When a request is made with patterns we assume the extension is 'dyalog' if no dot is present
          :If ∨/'?*'∊fname
              sw←⊂∆propagate'SOURCE NOLINK NONAME VERSION'
              :For i :In ⍳⍴allpaths
                  (path t ext)←1 splitName i⊃allpaths
                  (folder cname)←FS splitLast path
             ⍝ Because we don't know to which folder the pattern applies we grab everything
                  (t files)←↓⍉2↑[2]List({q,⍵,q←WIN/'"'}folder),' -raw -recursive -full=2 -extension'
                  :If found←~0∊⍴dir←0≠∊⍴¨t
                  :AndIf found←∧/(∨\dir)≥t←(¯8↑¨files)∊⊂'name.txt' ⍝ skip non scripted namespaces
                 ⍝ Any unscripted namespace?
                      name←{⍵↑⍨-⊥⍨~'/\'∊⍨⍵}¨files
                      ns←dir\1∊¨dir⊂name∊⊂'name.txt' ⍝ those are unscripted namespaces
                      :If ~1∊ns ⍝ if none exist
                          dir←{0}¨files←(~dir)∘/files ⍝ the easy case
                      :Else
                          keep←ns∨~dir ⋄ i←ns⍳1
                          :Repeat
                              isns←((⍴↑w)↑¨files)∊w←files[i],¨FS ⍝ namespace items
                              keep←keep>isns ⋄ ns←ns>isns ⋄ ns[i]←0
                          :Until (⍴ns)<i←ns⍳1
                          (files dir)←keep∘/¨files dir
                      :EndIf
                      t←(⍳⍴ns)=ns⍳ns←'\.\d+(\.[^.]+)$'⎕R'\1'⊢files ⍝ remove ver no
                      (files dir)←t∘/¨ns dir ⋄ name←{⍵↑⍨-⊥⍨~'/\'∊⍨⍵}¨files
                 ⍝ Select files from pattern
                      ext←'.',ext
                      :If cname∨.≠'*'
                          t←'^','\*' '\?' '\.'⎕R'.*' '.' '\\.'⊢cname ⍝ turn limited regex into full regex
                          i←1+t ⎕S 2 ⎕OPT WIN remExt¨name            ⍝ find the files matching the pattern
                          t←lCase¨⍣WIN⊢(-⍴ext)↑¨name[i]
                          i←⊂⊂(dir[i]∨t∊⊂lCase⍣WIN⊢ext)/i
                          (files dir)←i⌷¨files dir
                      :Else ⍝ grab all files with the wanted extension
                          t←lCase¨⍣WIN⊢(-⍴ext)↑¨files
                          c←dir∨t∊⊂lCase⍣WIN⊢ext
                          (files dir)←c∘/¨files dir
                      :EndIf
                 ⍝ Load files and namespaces separately
                      files←files,¨dir/¨⊂FS,cname,ext
                      :If ~0∊⍴files
                          ref←protect target∘Load¨'"',¨files,¨'"',¨sw ⍝ preserve some switches
                          ref←ref~⊂⍬ ⍝ remove namespaces not loaded
                      :EndIf
                      :Leave
                  :EndIf
              :EndFor
              →0
          :EndIf
         
    ⍝ Locate source
          xml←src←0
          :For i :In ⍳nf←⍴allpaths ⍝ search each folder
              (folder cname)←FS splitLast i⊃allpaths
              (cname t ext)←1 splitName cname
              :If 0<⍴1⊃(v ts)←folder ListVersionsTS cname,'.',ext
                  :If lastv
                      ⍙VERSION←⌈/v
                  :Else
                      lastv←t=⍙VERSION←⍙VERSION{⍺+⍵×⍺≤0}t←⌈/v  ⍝ accept neg ver
                      'version not found'⎕SIGNAL 922 if~⍙VERSION∊v~(1<⍴v)/0
                  :EndIf
                  :If lastv∧(∧/c∊v)∧2=⍴c←0∪t←⍙VERSION
                ⍝ Are we dealing with the last version and do we have an unnamed file?
                      t←1⍴c[⍒⊃ts[v⍳c]] ⍝ pick the most recently changed file
                  :EndIf
                  src←fixTabs splitOnNL name←folder,FS,cname,(dotVer t),'.',ext
                  :Leave
              :EndIf
          :EndFor
         
⍝ If the source was not found, that the name is a folder and that the switch
⍝ disperse is not set then we may be looking at a snapped non scripted ns:
          :If src ⍙DISPERSE≡0 0
          :AndIf ∨/dir←isDir¨allpaths
              files←'a'Dir(folder←((dir⍳1)⊃allpaths),FS),'*'
          :AndIf ∨/w←(1⊃files)<(4⊃files)∊⊂c←'name.txt' ⍝ signature of a ns saved by Snap
              (name v)←{s←¯1+⍵⍳' ' ⋄ ((-⊥⍨'.'≠t)↑t←s↑⍵)(s↓⍵)}GetUnicodeFile folder,FS,c
          :AndIf (∧/~'{∇⍎⎕'∊(t⍳1)↓v)∧1=+/t←'←'=v ⍝ validate system variables definition
              :If protect∧0≤target.⎕NC name
                  ref←'** "',('.',⍨⍕DF target),name,'" is already defined' ⋄ →0
              :Else
                  name target.⎕NS'' ⋄ name target.{⍺⍎⍵}v ⍝ create ns and set sys vars
                  :If 0<⍴dir←1⊃files←(~w)∘/¨files ⍝ remove the name file, extension, vernos
                      fname←0⍴files←(4⊃files)[⍋dir] ⍝ get files first
                      :If 0<w←+/~dir ⍝ any file to load?
                          fname←∪remVerno∘remExt¨w↑files
                      :EndIf
                 ⍝ We return a ref to the new namespace defined
                      ref←target⍎name
                      protect ref∘Load¨folder∘,¨(fname,w↓files),¨⊂∆propagate'SOURCE NONAME VERSION NOLINK'
                  :EndIf
                  →0
              :EndIf
          :EndIf
         
          →0 if okifempty∧src≡0
          t←~'.'∊msg←(2-isrel)⊃allpaths,⍨⊂cname
          msg←'cannot find the file "',msg,(t/'.',ext),'"',isrel/' in any of the SALT USER folders'
          msg ⎕SIGNAL 22 if src≡0 ⍝ file found? then 'name' is defined also
         
          ref←src ⋄ →0 if ⍙SOURCE≡1   ⍝ exit returning source (a VTV)
         
⍝ In 2013 we started fixing the fns with their original ts/an
⍝ The info is in the source. We extract it before going any further
          t←'⍝)('∘≡∘(3∘↑)¨src ⋄ info←t/src ⋄ src←(~t)/src
         
          isns←':⍝ '∨.=t←1↑rlb 1⊃src  ⍝ is this a namespace (as opposed to a fn)?
          isvar←'⌷'=t
         
⍝ We are now ready to FIX the object.
⍝ Some items (like Classes) may require other namespaces to be present,
⍝ if so, we need to bring them in BEFORE we fix the object (probably a Class):
         
          lines←⍬
          :If isns∨isvar<t←APLV≥15
              lines←('^.*?⍝(?i:([∇',t↓'!]):require +(?:file://)?)(?!.*?'')(=?\S+)\s*$')⎕S'\1\2'⊢src
          :EndIf
          :For line :In lines
         ⍝ If the line starts with ∇ we are using the pre V15 style where = means "same folder as mine", otherwise (!)
         ⍝ the path is ALWAYS relative, in other words: '∇/' or '!/' is absolute, '∇x' is SALT, '∇=' or ! is same
              needed←folder{~isRelPath 1↓⍵:1↓⍵ ⋄ >/b←'∇='=2↑⍵:1↓⍵ ⋄ ⍺,FS,(1+∧/b)↓⍵}line
              :Trap 22
                  1 target Load needed  ⍝ same location
              :Else
                  ⎕←'*** Required file ',needed,' not found'
              :EndTrap
          :EndFor
         
          :If nosrc←isns                  ⍝ ** [name]Spaces (Class 9) **
              nosrc←'no' 'NO'∊⍨⊂⍙SOURCE
              :If ⍙DISPERSE≡0 ⍝ normal Load
                  fix←⍙NONAME⍱nosrc
                  :Trap 0
                      :If protect∧fix ⍝ we need to find if the name is available
                          c←':class ' ':interface ' ':namespace '
                          :For w :In src
                              :If 4>i←1⍳⍨↑¨c⍷¨⊂lCase t←rlb w  ⍝ is it one of these?
                                  cname←¯1↓(t⍳' ')↑t←(⍴i⊃c)↓t ⍝ then grab the name
                                  :Leave                      ⍝ and skip the rest
                              :EndIf
                          :EndFor
                      :AndIf 0<target.⎕NC cname        ⍝ if it already exists
                          ref←'** "',cname,'" is already defined' ⋄ →0
                      :Else  ⍝ name is to be (re)defined
                     ⍝ Do we want a ref or no source? Then don't fix with name.
                          nref←ref←fix target.⎕FIX src ⍝ otherwise use the new name
                          :If nosrc
                              cname←(⍕DF target),'.',2⊃'.'splitLast⍕ref ⍝ find full target name
                              cname CopyNs ref         ⍝ perform copy
                          :EndIf
                     ⍝ We now have the ref defined, we now need to reset the fns' ts/an
                          nref fixTs info
                      :EndIf
                  :Else
                      ref←'*** Could not bring in <',name,'>:',⍕⎕DMX.(EM Message) ⋄ →0
                  :EndTrap
         
              :Else          ⍝ disperse the elements
         
        ⍝ There are 2 things to consider:
        ⍝ 1. the target must be there (this has been checked already)
        ⍝ 2. the elements needed must be in the source
         
                  nref←0 target.⎕FIX src ⋄ QV←⍬
                  :If ⍙DISPERSE≡1
                      needed←nref.⎕NL-2.1 9 3 4 ⋄ w←⍬ ⍝ everything
                      QV←'⎕IO' '⎕CT' '⎕ML' '⎕PP' '⎕FR' '⎕WX'
                  :Else                                             ⍝ only the names specified
                      QV←w/⍨t←'⎕'∊¨1↑¨w←⍙DISPERSE splitOn',' ⋄ needed←(~t)/w
                      (⍕'these names are not in the script:',t/needed)⎕SIGNAL 911 if∨/t←1>nref.⎕NC needed
                  :EndIf
            ⍝ OK, all seem valid, bring them in
                  :If protect∧∨/t←0≤target.⎕NC needed
                      ref←'**',⍕(t/needed)'already defined'
                      →0
                  :Else
                      target.⎕EX¨needed
                  :EndIf
                  :Trap 16 11 if 9∊⌊t←nref.⎕NC needed
                      ref←'*** Problem bringing in classes (derived classes?)'
                      :If ~0∊⍴cl←(9.4=t)/needed         ⍝ start with classes
                          target{⍺.⎕FIX ⎕SRC nref⍎⍵}¨cl
                      :EndIf
                      ext←'.',⍨⍕DF target
                      :For ns :In w←(nosrc∧9.1=t)/needed
                          :If (⍕nref⍎ns)≡(⍕nref),'.',ns ⍝ is this for real (not nested)?
                              cname←ext,ns              ⍝ find full target name
                              cname CopyNs nref⍎ns      ⍝ perform copy
                          :EndIf
                      :EndFor
                      ref←'*** Unable to comply because of sourced objects'
                      (⍕target)⎕NS'nref.'∘,¨needed~cl,w ⍝ bring them in
                      :If ~0∊⍴QV
                          target⍎⍕QV,'←',nref⍎⍕QV
                      :EndIf
                      ref←'* ',(⍕⍴needed),' objects dispersed in ',⍕target ⍝ return OK
                  :EndTrap
                  :Return                               ⍝ no need to stamp data
              :EndIf
         
          :ElseIf isvar              ⍝ ** Variables (Class 2) **
         
      ⍝ The result of <Load> can only be the name of the object or its rep/val if -noname
              nref←target(t←{1↓¯1↓⍵[⍳⍵⍳'←']}⎕IO⊃src)src
              (t ref xml)←target ⍙NONAME t protect fixVar src
              →⍙NONAME/0
         
          :Else                      ⍝ ** Fns/Ops (Class 3/4) **
         
      ⍝ Find the name of the fn/op (we could parse the text but we use ⎕FX)
              ns←⎕NS'' ⋄ fname←ns.⎕FX src  ⍝ define locally
              :If t←isChar fname
              :AndIf ⍙NONAME∨protect≤0=nc←target.⎕NC fname
                  :If ⍙NONAME
                      ref←ns.⎕OR fname ⋄ →0
                  :EndIf
                  target.⎕EX(nc∊2 9)/fname   ⍝ make sure we can fix in target
              :AndIf isChar ref←target.⎕FX src
             ⍝ Fix the ts/an
                  target fixTs info
                  nref←target fname src info
              :Else
                  ref←(1+t)⊃('*** could not fix <',name,'>')('** "',fname,'" is already defined')
                  →0
              :EndIf
          :EndIf
          v←(⍙VERSION⌈t)×¯1*t←~lastv ⍝ mark this version as a "requested previous version"
          :If ~⍙NOLINK∨nosrc∧isns    ⍝ asking for no source for a ns implies no link
              SetDelta nref name v((~⍙NONAME)/⍕ref)((1+xml)⊃'apl' 'xml')0
          :EndIf
     ⍝ End Load
        ∇


        ∇ tgt←{opt}Save arg;allver;but;cap;cmd;delims;ERR;ext;file;filename;fnname;folder;i;isFn;isVar;last;maxver;msg;name;named;nosource;ns;n0;origts;prev;r;ref;same;savename;src;t;there;usever;v;v0;xml;⍙BANNER;⍙CONVERT;⍙ENCRYPT;⍙FORMAT;⍙MAKEDIR;⍙NOPROMPT;⍙VERSION;⎕TRAP
          :Access Shared Public
⍝ SAVE object [to file]
⍝ opt allows to error out instead of not saving when prompted
         
          :If isHelp arg
              r←'Save object [filename]' ''
              r,←'Save object in a specific file (default same place if already SALTed)' '' 'Modifiers:'
              r,←⊂'-convert        Convert the namespace into source form if necessary'
              r,←⊂'-banner=        Add a banner at the top of the converted namespace'
              r,←⊂'-noprompt       Do not prompt for confirmation'
              r,←⊂'-makedir        Create all necessary directories'
              r,←⊂'-version[=]     Version number to save'
              r,←⊂'-format=        Save variable in specific format (APL or XML)'
              tgt←⊃r ⋄ →0
          :EndIf
         
          tgt←0 0⍴'' ⍝ assume it won't work
          :If 0=⎕NC'opt'
              opt←0
          :EndIf  ⍝ default no options
         
          ERR←900 ⍝ this is the error starting range we use if things go wrong
          but←'Yes' 'No',(1↑opt)/⊂'Cancel' ⍝ the buttons on forms
         
⍝ We accept strings containing a name AND a (ref string) argument
          isVar←0 ⍝ is this a variable?
         
⍝ There are 2 ways to call this fn:
⍝ Save ref ['location']    : ref is a class or ns, location is where to save (default as tagged)
⍝ Save 'object [location]' : object may be a path. If relative CRef provides the source ns.
⍝ object may be a class 2 3 4 9 non GUI or ⎕OR object
         
          :If isFn←isChar arg    ⍝ is this a normal argument?
              t←' '⍳⍨arg←rlb arg ⋄ fnname←rlb cmd←t↓arg ⋄ arg←(t-1)↑arg
              :If '['∊⍕ref←CRef  ⍝ unnamed (hopefully not renamed) ref?
                  :If 0∊t←'.'≠fnname←arg
                      ref⍎←(¯1+i←-t⊥t)↓arg ⋄ fnname←i↑arg
                  :EndIf
              :Else              ⍝ named ref
                  (t ref fnname)←{s←s/⍨s≢⍕r←⍎s←⍵↓⍨¯1+i←-⊥⍨⍵≠'.' ⋄ s r,⊂i↑⍵}(⎕IO⊃⎕NSI){'#.'≡2↑⍵:⍵ ⋄ ⍺,'.',⍵}arg
              :EndIf
              n0←ref fnname
              :If (t←|ref.⎕NC⊂fnname)∊9.1 9.4 9.5
                  n0←DF ref←ref⍎fnname ⋄ isFn←0
              :Else
                  :If isVar←2∊⌊t
                      isVar←~valWithRef ref⍎fnname
                  :EndIf
                  'Invalid object'⎕SIGNAL ERR+2 if isVar⍱isFn←t∊3.1 3.2 4.1 4.2
              :EndIf
          :Else                  ⍝ (ref name) pair: ref is a class or a ns to save
              'Invalid argument'⎕SIGNAL 5 if 2<⍴,arg
              fnname←n0←⍕⍬⍴(ref cmd)←2↑arg,⊂''   ⍝ fully qualified nspace name
          :EndIf
         
          ⎕TRAP←_TRAP
          savename←0 fixFsep 1⊃1↑'-version[=] -makedir -format= -noprompt -convert -banner='∆parse cmd'1SL'
          :If ∨/t←'='FS=¯1↑savename ⍝ using .../= as filename means use the same name as the object
              savename←((-1↑t)↓savename),'.'afterLast⍨'.',⍕fnname
          :EndIf
          usever←1≡⍙VERSION←⌊|num ⍙VERSION ⍝ -ver alone means "use version #s"
         
⍝ Rules:
⍝ If a filename  is supplied we use it else we use the one linked to the object
⍝ If a version # is supplied we use it to possibly overwrite the file
⍝ If only -version (no =) then we start using version #s if not already the case
⍝ Without it we either overwrite or continue using version #s
         
          :If named←0≠⍴filename←savename ⍝ file name specified
              filename←(1⊃getSetting'workdir')ClassFolder savename ⋄ (v0 origts)←¯0.5 ''
          :EndIf
         
     ⍝ Get the SALT info if any
          :If isFn
              'Fn is not SALTed'⎕SIGNAL 911 if named<0∊⍴v←fnData(ref.⎕NR fnname)fnname
          :ElseIf isVar
              'Variable is not SALTed'⎕SIGNAL 911 if named<0∊⍴v←varData⊂ref fnname
          :Else ⍝ ref
              'Save cannot be used on SALT itself'⎕SIGNAL 911 if named<{0::0 ⋄ ⎕THIS≡⍵}ref ⋄ v←⍬
              :If 9.1=ref.⎕NC⊂'SALT_Data'
                  v←ref.SALT_Data
              :ElseIf ~named
                  'Ref does not point to a SALTed namespace'⎕SIGNAL 911
              :EndIf
          :EndIf
     ⍝ If no filename has been supplied and we have data we use that name
          :If same←named⍱0∊⍴v
              filename←v.SourceFile
          :EndIf
          :If ~0∊⍴v  ⍝ we have data
              :If same
              :OrIf {WIN:≡/lCase¨¨⍺ ⍵ ⋄ ⍺≡⍵}/1 0 1∘/∘splitName¨filename v.SourceFile ⍝ then if the names match
                  filename←v.SourceFile ⋄ origts←⍕v.LastWriteTime ⋄ v0←v.Version     ⍝ then record this
              :EndIf
          :EndIf
          (filename t ext)←splitName filename ⍝ get rid of the verno
          (folder name)←FS splitLast filename
         
⍝ Find the version number to use
          maxver←⌈/0,allver←folder ListVersions name,'.',ext
          :If usever∨(⍙VERSION≡0)∧maxver>0
              ⍙VERSION←1+maxver
              :If ⍙NOPROMPT<(v0<maxver)∧v0>0 ⍝ we should be the last version
                  cap←'This is not the last version!'
                  →0 if 2=t←msgBox cap('Continue?')'Warn'but
              :EndIf
          :EndIf
         
          cap←'Save ',24{⍺>⍴⍵:⍵ ⋄ '...',(3-⍺)↑⍵}filename
          :If ⍙NOPROMPT<there←⍙VERSION∊allver
              →0 if 2=t←msgBox cap('Confirm overwrite of ',({0≡⍵:'file' ⋄ 'version ',fmtVersion ⍵}⍙VERSION),'?')'Warn'but
              'abort'⎕SIGNAL ERR+1 if t=3
         ⍝ OK, we have permission to overwrite, let's make sure it hasn't been overwritten by someone else first
          :AndIf (v0≠¯0.5)∧⍙VERSION≥v0 ⍝ don't if we are trying to replace a lower version
              t←⍕lastWrTime file←folder,FS,name,(dotVer ⍙VERSION),SALTEXT{0∊⍴⍵:⍺ ⋄ '.',⍵}ext
          :AndIf origts{⍺≢⍵:6<⍴⍺∪⎕D ⋄ 0}t   ⍝ not same as we loaded?
              t←{0∊⍴⍵:'No timestamp found for original file!' ⋄ 'Now dated ',⍵}t
              t←file''({0<⍴⍵:'Was dated ',⍵,' when loaded.' ⋄ 'No previous date available.'}origts)(t)'Proceed anyway?'
              →0⍴⍨1≠msgBox'Source file timestamp has changed...'t
          :EndIf
⍝ The file may not be there but the version to use is less than the highest version
          :If ⍙NOPROMPT<there<⍙VERSION<maxver ⍝ Version is LESS than highest version number
              →0 if 1≠msgBox cap('Note: highest existing version is number ',(fmtVersion maxver),'. Proceed anyway?')'Warn'
          :EndIf
         
          (last tgt)←(folder,FS,name)∘,¨(0 1/¨⊂dotVer ⍙VERSION),¨⊂SALTEXT{0∊⍴⍵:⍺ ⋄ '.',⍵}ext
          xml←nosource←0
          :If isFn
              'function is locked'⎕SIGNAL 911 if 0∊⍴src←remTag ref.⎕NR fnname
              :If 3.2∊ref.⎕NC⊂fnname  ⍝ Older version of idioms still have no ← on the first line
                  src[⎕IO]←⊂((>/t⍳'←{')/fnname,'←'),t←⎕IO⊃src
              :EndIf
              ref←ref fnname src ⍬
          :ElseIf isVar
              :If 0≡xml←⍙FORMAT ⍝ if not set use global setting
                  :If 0∊⍴t←varData⊂n0 ⍝ do we know this var?
                      xml←'xml'≡getSetting'varfmt' ⍝ no, use global setting
                  :Else ⍝ yes, use last saved format
                      xml←'xml'≡t.Format
                  :EndIf
              :Else
                  xml←'xml'≡t←lCase xml
                  '-FORMAT=APL or XML only'⎕SIGNAL 911 if⍱/'xml' 'apl'∊⊂t
              :EndIf
              :Trap nosource←0
                  src←fnname xml ∆VCR ref⍎fnname
                  ref←ref fnname src
              :Else
                  'Unable to create source for variable'⎕SIGNAL ERR+2
              :EndTrap
         
          :Else ⍝ NS: we grab the source; if it does not exist we create one
              :If nosource←0≡src←{16::0 ⋄ ⎕SRC ⍵}ref
⍝ For the moment we bring in the code to convert the namespace
                  bringConvertCode
⍝ Make sure the ref has a proper name
                  ref.⎕DF n0
                  :Trap 0 ⍝ there could be a number of reasons for this to fail
                      :If ⍙BANNER≢v←0
                          :If '⍎'=1↑v←⍙BANNER
                              v←⎕FMT CRef⍎1↓v
                          :EndIf
                      :EndIf
                      ref.⎕EX t if 9∊ref.⎕NC t←'SALT_Data' ⍝ in case we dealing with an ex SALTed ns
                      src←(0 0 v fnname)⎕SE.Dyalog.Convert.ntgennscode src←ref
⍝ Convert the namespace if required
                      :If ⍙CONVERT
                          ref←ref.##.⎕FIX src ⋄ nosource←0
                      :EndIf
                  :Else
                      'Unable to convert namespace'⎕SIGNAL ERR+3
                  :EndTrap
              :EndIf
          :EndIf
         
     ⍝ In 2013 we started saving program ⎕AT info after the script
          t←⎕SE.SALTUtils.(SETTS SETCOMPILED) ⍝ is it turned on?
          :If (∨/t)>1∊'/SALT/'⍷{s←'\'=v←⍵ ⋄ (s/v)←'/' ⋄ v}tgt ⍝ except for SALT
              :If isFn
                  :If 3.1=ref[1].⎕NC⊂fnname
                      ref[4]←⊂src,←ref[1]getTs,⊂fnname
                  :EndIf
              :ElseIf ~isVar
              :AndIf ~0∊⍴t←ref.⎕NL ¯3.1
                  src,←ref getTs t
              :EndIf
          :EndIf
         
          :Trap 0
              {}makeDir⍣⍙MAKEDIR⌷pathOf t←tgt
              ns←mergeTxt src
              ns PutUTF8File t  ⍝ 't' used below if error
⍝ do not update current file if not the latest version
              :If (maxver≤⍙VERSION)∧tgt≢t←last
                  ns PutUTF8File t
              :EndIf
          :Else
              msg←⎕DMX.Message
              :If ⎕EN=90
                  msg←⎕EXCEPTION.Message
              :EndIf
              ('Unable to create file ',t,': ',msg)⎕SIGNAL 22
          :EndTrap
          →0 if nosource
          SetDelta ref tgt ⍙VERSION({0::'' ⋄ ⍵.SALT_Data.GlobalName}ref)(3↑(3×xml)↓'aplxml')0
     ⍝ End Save
        ∇

        ∇ {r}←bringConvertCode;t
          :If r←0=⎕SE.⎕NC ¯6↓t←'Dyalog.Convert.CDate'
          :OrIf r←0∊⎕SE.⎕NC t
          :OrIf r←(⎕SE⍎t)≠100⊥3⍴⎕TS
              'Dyalog'⎕SE.⎕NS'' ⍝ ensure parent there
              ⎕SE.Dyalog.Convert←Load'[SALT]/lib/NStoScript -noname'
              ⎕SE⍎t,'←100⊥3⍴⎕ts'
          :EndIf
        ∇

        ∇ select←la SnapGetPatterns select;ex;ss;t;pat;objs;patterns
    ⍝ Look at patterns
          (objs patterns)←la
          :If (∨/select)∧0≢patterns
              ex←'~'∊1↑patterns   ⍝ are we reversing the behaviour?
              ss←select/objs      ⍝ the strings (objects) to search
              :If ∨/'?*'∊patterns ⍝ looking for a limited regex?
             ⍝ Pattern holds the list of names or patterns, a la DOS, to Snap
             ⍝ A pattern is assumed to be anchored at the beginning and end,
             ⍝ '?' means "any character" and '*' means any sequence, e.g. A*B is ^A.*B$
                  t←'\?' '\*'⎕R'.' '.*'⊢patterns~'~' ⍝ turn limited regex (DOSlike) into full regex
                  pat←{⎕ML←3 ⋄ '$',⍨¨'^',¨(⍵≠' ')⊂⍵}t
             ⍝ Find the list to process
                  select\←ex≠ss∊t←pat ⎕S'\0'⊢ss
              :Else ⍝ simple case
                  select\←ex≠ss∊pat←{⎕ML←3 ⋄ (⍵≠' ')⊂⍵}patterns~'~'
              :EndIf
              :If ~∨/select
                  Warn'pattern',((1⌈2⌊⍴pat)⊃' does not' 's don''t'),' match any object'
              :EndIf
          :EndIf
        ∇

        ∇ nEw←objs newName ex;t;fr;to;b;tO;new;i
     ⍝ Find a filename for objects; ex are the characters for ∆ and ⍙ (default ^ and =)
          t←'0123456789abcdefghijklmnopqrstuvwxyz' ⋄ ex←ex,(⍴ex)↓'^=_'
          fr←t,'àáâãåäèéêëæíîïìòóôõöøùúûüñþðßçÇABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÅÄÈÉÊËÆÌÍÎÏÒÓÔÕÖØÙÚÛÜÐÝÑ∆⍙'
          to←t,'aaaaaaeeeeeiiiioooooouuuunpdsccabcdefghijklmnopqrstuvwxyzaaaaaaeeeeeiiiioooooouuuudyn',ex
          tO←t,'aaaaaaeeeeeiiiioooooouuuunpdscCABCDEFGHIJKLMNOPQRSTUVWXYZAAAAAAEEEEEIIIIOOOOOOUUUUDYN',ex
          new←{to[fr⍳⍵]}¨objs ⋄ nEw←{tO[fr⍳⍵]}¨objs
     ⍝ There may be names which will cause conflicts under OSs like Windows
          b←((3↑¨new)∊'com' 'lpt')∨new∊'aux' 'con' 'nul' 'prn'
          :If ∨/b←b∨{(⍳⍴⍵)≠⍵⍳⍵}new ⍝ take into account ALL names
              new←new,¨i←b/¨'-',¨⍕¨b\⍳+/b
              nEw←nEw,¨i
          :EndIf
        ∇

⍝ With Save defined we write a Snap command like this:
        ∇ names←{tref}Snap arg;AllObjs;allowed;b;body;clall;class;diff;difffn;diffvar;ex;excludelist;ext;file;files;fnames;fr;i;lx;makefn;name;nc;new;nr;nsn;objs;pat;Path;rec;refName;root;select;srcf;show;some;ss;Sw;t;to;tref;txt;unable;vov;vv;Warn;ws;⍙BANNER;⍙CLASS;⍙CONVERT;⍙ENCRYPT;⍙FILEPREFIX;⍙FORMAT;⍙LOADFN;⍙MAKEDIR;⍙NOPROMPT;⍙NOSOURCE;⍙PATTERNS;⍙SHOW;⍙VERSION;⍙∆⍙;nola;cancelled;r;list;withver;Q;Quote;using;refNameD;⎕PP;⍙CLEAN
          :Access shared public
          :If isHelp arg
              r←'Snap [path] (default current workdir)' '' 'Save all new or modified SALT objects in path' '' 'Modifiers:'
              r,←⊂'-class=       classes of objects to save (2 3 4 or 9) (*)'
              r,←⊂'-clean        start with a clean slate, remove all tags first'
              r,←⊂'-convert      convert namespaces into source form if necessary'
              r,←⊂'-banner=      Add a banner at the top of each converted namespace'
              r,←⊂'-fileprefix=  filename prefix of objects to snap'
              r,←⊂'-format=      save variable in specific format (APL or XML)'
              r,←⊂'-loadfn[=]    the path (default =arg) where to store the fn to rebuild the ws'
              r,←⊂'-nosource     do not bring in the source of namespaces with the LOAD fn'
              r,←⊂'-noprompt     skip confirmation prompt'
              r,←⊂'-makedir      create any necessary folder'
              r,←⊂'-show[=]      do not save, show what would be saved'
              r,←⊂'-patterns=    patterns of objects to save (*)'
              r,←⊂'-version[=]   version number to save'
              r,←⊂'-∆⍙=          characters to use in filenames for objects having these characters'
              r,←⊂'* Note: to exclude simply prefix with "~"'
              names←⊃r
              →0
          :EndIf
          Path←specialName fixFsep 1⊃1↑'-Banner= -Class∊ ~.,12439 -Clean -Convert -FilePrefix= -Format= -LoadFn[=] -MakeDir -NoPrompt -NoSource -Patterns= -Show[=] -Version[=] -∆⍙='∆parse arg'1SL'
         
          Warn←{⍺←'** WARNING: ' ⋄ ~0∊⍴⍵:⎕←⍺,⍵} ⍝ should this go to the status window?
         
          :If nola←0∊⎕NC'tref' ⍝ a left arg means we return 2 things: the names and their files
              tref←CRef        ⍝ this is used when making recursive calls
          :EndIf
          :If 326≠⎕DR refName←tref ⍝ find the display forms (2, one with a dot) of the target reference
              refNameD←'.',⍨refName ⋄ t←⍕tref←⍎refName ⋄ names←⍬(0 2⍴0)
              nc←refName{'[Namespace]'≡nm←(n←+/∨\'.'=⌽⍵)↓⍵}t
              Warn(nc<refName≢t)/'namespace ',refName,' is a reference to ',t
          :Else
              refNameD←'.',⍨refName←DF tref
          :EndIf
          unable←names←⍬ ⍝ return list of objects saved + files if la supplied
         
     ⍝ If the path ends with '=' we append the name of this ws
          :If '='∊¯1↑Path
              Path←(¯1↓Path),(-⊥⍨FS≠t)↑t←⎕WSID
          :EndIf
         
     ⍝ Load program: the name of the pgm can be the same has its file whose path can be specified.
          :If makefn←⍙LOADFN≢0
              'Invalid ⎕LX: multiple lines in it'⎕SIGNAL 911 if 1<1↑⍴⎕FMT ⎕LX ⍝ could be a CR/NL embedded string
              :If ⍙LOADFN≡1
                  ⍙LOADFN←Path
              :EndIf
              :If '='∊1↑⍙LOADFN  ⍝ if the name starts with '=' we prepend the Path
                  ⍙LOADFN←Path,1↓⍙LOADFN
              :EndIf
              :If isRelPath ⍙LOADFN
                  ⍙LOADFN←Path{⍺,(0=⍴⍺)↓FS,⍵}⍙LOADFN
              :EndIf
         ⍝ If the path contains a '+' it means we want to generate extended code
              b←'+'∊⍙LOADFN
              ⍙LOADFN←specialName fixFsep ⍙LOADFN~'+'
        ⍝ The name of the file may be supplied with an extension:
              ext←'' ⋄ name←'load_ws' ⍝ default
              :If '.'∊2⊃t←FS splitLast ⍙LOADFN
                  ⍙LOADFN←1⊃t ⋄ (name ext)←'.'splitLast 2⊃t
              :EndIf
              ('Invalid load fn ',(1+t)⊃'name' 'extension')⎕SIGNAL 911 if(0>⎕NC name)∨t←~(⊂ext)∊'dyapp' '',⊂1↓SALTEXT
              ⍙LOADFN←⍙LOADFN name ext b
          :EndIf
         
          root←#=tref
          excludelist←'Ûcmd' 'Ûargs' 'Ûriu' 'SALT_Data',root/⊂'SALT_Var_Data'
         
     ⍝ Find objects to snap. We allow ~ to precede the class selection to mean 'exclude'
          ⍙CLASS←⍙CLASS{⍵:⍺~'~' ⋄ ⍺}ex←'~'∊⍙CLASS
          class←⍙CLASS ∆default 2 3 4 9
     ⍝ Expand to subclasses
          allowed←∊t←2.1(3.1 3.2)(4.1 4.2)(9.1 9.4 9.5)
         
     ⍝ This version does not allow fn refs (Save won't have it) which are class 3.3
     ⍝ If we are to produce a load fn and some objects won't be saved we tell the user:
          :If makefn∧0<⍴b←(tref.⎕NL-2 3 4 9.2)~AllObjs←tref.⎕NL-allowed
              Warn((⍕⍴b),' object',(1∊⍴b)↓'s of class ',⍕∪tref.⎕NC b),' won''t be dealt with'
          :EndIf
          AllObjs~←excludelist    ⍝ remove here
          class←allowed∩class,∊(2 3 4 9∊class)/t
          class←allowed~⍣ex⊢class ⍝ ~ means exclude
         
          {}makeDir⍣⍙MAKEDIR⊢Path ⍝ ensure there
         
          →0 if nola∧makefn<0=nr←+/select←AllObjs ⍙PATTERNS SnapGetPatterns(tref.⎕NC AllObjs)∊class
         
     ⍝ Clean the space first if necessary
          :If ⍙CLEAN∧⍙SHOW≡0
              {}0 cleanWS tref
          :EndIf
         
     ⍝ AllObjs is the entire list of objects in the current ns (usually #)
     ⍝ clall   is the class of each
     ⍝ select  is the mask of selections
     ⍝ unable  is the mask of objects unable to save (e.g. locked fn, or refs)
         
     ⍝ All the names we have now are of a valid class. Let's see which ones are different.
     ⍝ We warn when some objects cannot be saved, for ex, some fns may be locked.
          unable←b\{0∊⍴tref.⎕VR ⍵}¨AllObjs/⍨b←3.1=clall←tref.⎕NC AllObjs
          :If ∨/select>←b←unable∧ss←select∨makefn
              Warn'these fns are locked in ',refName,':',⍕b/AllObjs
          :EndIf
     ⍝ Some vars may be "unsavable":
          :If ∨/vov←t←ss∧clall=2.1 ⍝ Valid Or Variable
              (b vv)←↓⍉⊃{6::0 0 ⋄ 1(tref⍎⍵)}¨t/AllObjs ⍝ could be undefined (e.g. ⎕SVO)
              unable∨←vov←t\~b
          :AndIf ∨/b
              unable∨←vov←vov∨t\b\valWithRef¨tref⍎¨b/t/AllObjs
          :EndIf
          select>←vov
         
     ⍝ Have any of these been modified or are new? A non-scripted ns will appear as new.
     ⍝ Note that it is possible for a class not to have a script (e.g. a 'subform' is class that can be used as base to a class)
          nr←diff←srcf←⍬
          :If ∨/select
              nr←select/AllObjs
              :If ⍙CLEAN
                  diff←{1}¨srcf←{⍬}¨nr
              :Else
                  difffn←{0∊⍴nr←fnData ⍵ ⍬:1 '' ⋄ (nr.CRC≢calcCRC remTag ⍵)nr.SourceFile}
                  diffvar←{0∊⍴nr←varData⊂⍵:1 '' ⋄ xml←'xml'≡nr.Format ⋄ (nr.CRC≢calcCRC ⍺ xml ∆VCR{1=≡⍵:⍎⍵ ⋄ ⊃⍎/⍵}⍵)nr.SourceFile}
         ⍝ See if they are different - or new
                  (diff srcf)←↓⍉⊃(select/clall){2=cl←⌊⍺:⍵ diffvar tref ⍵ ⋄ 9≠cl:difffn tref.⎕NR ⍵ ⍝ var or pgm
                      16::(1-2×⍺=9.4)'' ⋄ cc←calcCRC ⎕SRC t←tref.⍎⍵                               ⍝ ref CRC
                      6::1 '' ⋄ (src crc)←t.SALT_Data.(SourceFile CRC)                            ⍝ old name & crc
                      (crc≢cc)src}¨nr
              :EndIf                                              ⍝ compare
          :EndIf
         
          :If ∨/vov←(∨/select)∧vov∨select\~b←0≤diff
              Warn'these objects cannot be saved in ',refName,':',⍕∪vov/AllObjs ⋄ select>←vov ⋄ (nr diff srcf)←b∘/¨nr(|diff)srcf
          :EndIf
         
     ⍝ This is the list of all objects to process. Whether we do it depends on -show.
          files←1 2⍴refName(srcf,⍪refNameD∘,¨nr)
         
          :If makefn⍱some←∨/diff  ⍝ anything to do?
              →nola/0             ⍝ should we return the filenames too?
              →0⊣names←names files
          :EndIf
         
          show←⍙SHOW≢0 ⋄ cancelled←0
         
          Q←'''' ⋄ Quote←{Q,((1+⍵=Q)/⍵),Q}
         
          :If some∨⍙CLEAN
         
     ⍝ We found there were objects that need saving. We will put them back where they belong
     ⍝ or we will put them in the path specified as argument.
     ⍝ If a relative path was specified we assume it is in the current workdir.
              :If isRelPath Path
                  Path←(⎕IO⊃getSetting'workdir')ClassFolder Path
              :EndIf
              Path←Path,(FS=¯1↑Path)↓FS,⍙FILEPREFIX~0
              'Cannot Snap into SALT folder (use a different location)'⎕SIGNAL 911 if(⎕SE.SALTUtils.getEnvir'DYALOG'){⍺≡(⍴⍺)↑⍵}Path
         
              names←diff/select/AllObjs
         ⍝ Find file names - replace chars in names which cause problems with the OS
              :If ∨/new←' '∧.=¨fnames←diff/srcf ⍝ reuse these filenames; note empty ones
            ⍝ Some objects are new and have no filename yet
                  fnames←AllObjs newName ⍙∆⍙ ∆default'%=' ⍝ we need to take them ALL into account
                  fnames←Path∘,¨new/diff/select/fnames,¨⊂SALTEXT ⋄ nc←⍴SALTEXT
            ⍝ There is a chance those new names are still not unique:
                  t←{⍺,'.',⍵}/1 0 1/⊃splitName¨srcf ⍝ remove ver no if any
                  :While ∨/b←fnames∊t
                      (b/fnames)←nc⌽¨(⍕¨⍳+/b),⍨¨(-nc)⌽¨b/fnames
                  :EndWhile
                  fnames←(fnames,(~new)/diff/srcf)[⍋⍒new] ⍝ reinsert old items in list
                  srcf[new/diff/⍳⍴diff]←new/fnames
                  files[1;2]←⊂srcf,0 1↓2⊃files[1;]
              :EndIf
         ⍝ We now have a filename for each object to be saved
         
              ss←⍙CONVERT<9.1=tref.⎕NC names ⍝ find scripted spaces to be written out
              :If 1∊ss ⍝ find which ones are scripted
                  ss←ss\tref{0::1 ⋄ 0⊣⎕SRC ⍺⍎⍵}¨ss/names
              :EndIf
         
         ⍝ We're all set. If we only SHOW we return the names right away.
              :If show ⍝ names, ss, srcf and fnames are in sync
             ⍝ If details requested show the filename where each object will end up
                  :If 'details'≡lCase⍕⍙SHOW
                      fnames←⊃splitName¨diff/srcf ⍝ split into name, version, extension
                      :If ∨/0 1∊b←⍙VERSION
                          fnames[;2]←(ss<b∨×t)/¨⍕¨1+t←⍎¨'0',¨fnames[;2]
                      :Else
                          fnames[;2]←(~ss)/¨⊂⍙VERSION
                      :EndIf
                      fnames[;3]←(~ss)/¨fnames[;3]
                      names←names,¨{' →',¯1↓(~'..'⍷⍵)/⍵}¨{⍺,'.',⍵}/fnames,'.'
                  :EndIf
         
              :Else  ⍝  ***  save and store the effective file name  ***
                  :If '⍎'∊1↑⍙BANNER
                      ⍙BANNER←'⍎',refNameD,1↓⍙BANNER
                  :EndIf
                  Sw←∆propagate'VERSION NOPROMPT CONVERT BANNER FORMAT'
                  {}makeDir∘pathOf⍣⍙MAKEDIR⌷Path
                  rec←⍬
                  (fnames names)←(fnames names){⍺[⍵]}¨⊂⍋ss ⍝ we do the non-scripted folders last (makes life easier if user cancels)
                  :For i :In ⍳nc←+/~ss ⍝ we have to loop to trap Cancel appropriately
                      :Trap 901 902
                          names[i]←fnames[i]{⍵/⍨~0∊⍴1 Save refNameD,⍵,' "',⍺,'"',Sw}¨names[i]
                      :Case 901
                          names[i]←⊂'' ⋄ →EndFor⊣cancelled←1
                      :Else
                          names[i]←⊂''
                      :EndTrap
                  :EndFor
                  :For i :In nc+⍳+/ss ⍝ non scripted nss here
                      file←remExt i⊃fnames ⋄ name←refNameD,i⊃names
                      rec,←1⊃t←name Snap file,Sw,' -makedir' ⍝ names and location are returned
                      files⍪←2⊃t ⍝ this is a LIST of files, it must match the names
                      files⍪←refName(1 2⍴file name)
                 ⍝ We put the name of the namespace in the folder
                 ⍝ We also put the system vars
                      using←{(~0∊⍴⍵)/1⌽') (',⍕Quote¨⍵}name⍎'⎕using'
                      t←1⌽')(⎕IO ⎕ML ⎕WX ⎕CT ⎕PP',(~0∊⍴using)/' ⎕USING' ⍝ ⎕RL skipped because of new format
                      t←name,' ',t,'←',(⍕5↑name⍎t),using
                      t PutUTF8File file,'/name.txt'
                      →EndFor if cancelled←901=⎕EN   ⍝ did the user cancel while in Snap?
                  :EndFor
         
         EndFor:
            ⍝ If the user cancelled we return what we've done so far
                  names←(refNameD∘,¨names[⍳nc⌊i-cancelled∧i≤nc]~⊂''),rec ⍝ 'i' was the loop counter
                  :If ~nola
                      names←names files
                  :EndIf
              :EndIf
          :EndIf
         
⍝                            ****  Produce a program to reload the code here  ****
          :If makefn>cancelled
    ⍝ Produce a fn to reload the entire ns
    ⍝ This may be impossible if the ns includes instances (e.g. GUI objects) or refs
              t←((~root)/'namespace ',refName,' in '),'ws ',⎕WSID
              body←0⍴ws←'⍝ This will recreate ',t,' as it was on',⍕'/:'{1↓⊃,/⍺,¨⍕¨⍵}¨↓2 3⍴⎕TS
              ws←ws'Load←{''**''≡2↑⍕s←⎕SE.SALT.Load ⍵:⎕←s} ⍝ used to verify SALT.Load''s result'
         
    ⍝ We show the names of the objects we cannot bring back, if any, and limit the size of the lines to display to 90
              :If 1∊unable                                       ⍝     90 is max width for display of objects names
                  body,←'' '⍝ These objects are not recreated:','⍝',¨⍕¨90 Fold unable/AllObjs
              :EndIf
              body,←root/''('#.(⎕IO ⎕ML ⎕WX ⎕CT ⎕PP)←',⍕#.(⎕IO ⎕ML ⎕WX ⎕CT ⎕PP))
              body,←root/(~0∊⍴t)/'#.⎕USING←',⍕Quote¨t←⎕USING
              body,←(~root)/⊂Q,refName,Q,' ⎕NS ',Q,Q
              :If ⍙LOADFN[4]∧0<⍴t←root↓files[;1] ⍝ extended form?
                  ex←'' '' '⍝ Recreate the non scripted namespaces'
                  body,←1⌽ex,{(Q,⍵,''' ⎕NS ⍬ ⋄ ',⍵,'.(⎕IO ⎕ML ⎕WX ⎕CT ⎕PP)←',⍕1↓t),(~0∊⍴U)/' ⋄ ',⍵,'.⎕USING←',⍕Quote¨U←1⊃t←⍎⍵,'.(⎕using ⎕IO ⎕ML ⎕WX ⎕CT ⎕PP)'}¨t
              :EndIf
        ⍝ Discard extra file info if we're not in extended mode
              b←~⍙LOADFN[4]
              files←1↑⍣b⊢files ⍝ only the first row contains all names
              Warn(b<⍙SHOW≢0)/'Loadfn program will be incomplete because the full snapping wasn''t done and filenames were not determined'
         
        ⍝ We could reuse fnames and names above but if -show was set they won't exist
              (fnames objs)←↓⍉↑⍪/files[;2] ⋄ r←(∊1↑¨⍴¨files[;2])/files[;1] ⍝ 'r' is taRget
    ⍝ We now have the entire list of objects, some may be non-scripted nss if we are NOT in extended mode in which
    ⍝ case we keep only the names and leave Load the trouble of finding what's inside.
              (fnames objs r)←(~objs∊files[;1])∘/¨(fnames objs r)
         
         ⍝ Find which version they are
              :If 0<⍴files←remVerno¨t←remExt¨fnames⊣withver←⍬
                  files←files{'"',⍺,'"',(0<⍴⍵)/' -version=',⍵}¨ex←(1+⍴¨files)↓¨t ⋄ withver←0<↑,/⍴¨ex
              :EndIf
        ⍝ We have to order the Loads according to class (e.g. Interfaces first)
              t←files
              (files r objs nc withver)←tref reOrderLoads(files r objs withver)
              ⍙LOADFN[4]∨←t≢files ⍝ will we have to use extended form? (yes if order has changed)
              ⎕PP←17
        ⍝ Add ⎕LX if not empty
              :If ⎕LX≢lx←''
                  lx←root↓' ',⊂1⌽'''#⍎⎕LX←''',(1+⎕LX=Q)/⎕LX
              :Else
                  Warn'⎕LX is empty'
              :EndIf
              (fr name ext ex)←⍙LOADFN
              {}makeDir⍣(⍙MAKEDIR>show)⌷fr
              :If (⊂ext)∊1↓¨SALTEXT'' ⍝ create a fn?
                  ex←((⍙NOSOURCE∧nc=9.1)/¨⊂' -source=no'),¨(withver<0≢⍙VERSION)/¨⊂' -version=',⍕⍙VERSION
                  'to'⎕NS'' ⋄ t←(12{(⍺×⌈(⍴⍵)÷⍺)↑⍵}¨files,¨ex,¨' -target='∘,¨r,¨Q),¨' ⍝ '∘,¨objs
             ⍝ There is no point switching to another space with ⎕CS because ASA we are out of the program it will be lost
                  {}÷name≡t←to.⎕FX(⊂name,';Load'),ws,body,('Load '''∘,¨t),(root/' ',⊂'⎕WSID←''',⎕WSID,Q),lx
                  :If show
                      ⎕←to.⎕VR name
                  :Else
                      {}Save'to.',name,' ',fr{⍺,(⍺∧.=' ')↓⍵}FS,name,' -noprompt -makedir'
                  :EndIf
              :ElseIf ext≡'dyapp' ⍝ create a boot file
                  t←remVerno∘remExt¨files
                  txt←('Load '∘,¨files),(⊂1⌽'''Run ⎕cs # ⋄ ⎕wsid←''',⎕WSID),'Run '∘,¨lx
                  :If show
                      ⎕←⊃txt
                  :Else
                      (mergeTxt txt)PutUTF8File fr,FS,name,'.dyapp'
                  :EndIf
              :EndIf
          :EndIf
     ⍝ End Snap
        ∇

          baseClass←{11 3::⍺ ⋄ bc←2 1⊃⎕CLASS ⍵
              bc←bc/⍨{0::0 ⋄ ⍵≡⍎⍕⍵}¨bc
              ⍺,bc}

        ∇ (files target objs nc wv)←tref reOrderLoads(files target objs wv);nc;b;list;t;class;norel;remove;cl0
    ⍝ The order in which the classes are loaded is important.
    ⍝ Classes that depend on other classes either because they are derived from them or because
    ⍝ they reference them through the :Signature statement must be loaded AFTER those referenced classes.
         
          :If ~0∊⍴objs⊣nc←⍬
          :AndIf ∨/b←9.4=nc←⎕NC objs
        ⍝ We must also take :Signature into account
              list←{'[]←'~⍨∊'^ *:Signature (\S+ *←)? *\S+ *(.*)$'⎕S',\1,\2'⎕OPT 1⊢⎕SRC ⍵}¨cl0←class←⍎¨b/objs
              list←tref.{0∊⍴⍵:⍬ ⋄ ⎕ML←3 ⋄ e←{(+/∧\' '=⍵)↓⍵}¨((⍵≠',')⊂⍵)~⊂''
                  ⊃,/{{6::⍬ ⋄ ⍎⍵}(⍵⍳' ')↑⍵}¨e}¨list
              list←list baseClass¨class
        ⍝ We now have all that is required for each class to be defined
              t←⍬
              :Repeat
                  t,←remove←class/⍨norel←{0∊⍴⍵}¨list
                  'circular reference: unable to create load fn'⎕SIGNAL 11↓⍨1∊norel
                  (class list)←(~norel)∘/¨class list
              :Until 0∊⍴list←list~¨⊂remove
              (files objs target nc wv)←(files objs target nc wv)⌷¨⍨⊂⊂⍋b\t⍳cl0
          :EndIf
        ∇

        ∇ {r}←RemoveVersions name;allver;b;cf;fc;folder;last;list;t;v;⍙ALL;⍙COLLAPSE;⍙NOPROMPT;⍙VERSION;⎕TRAP
          :Access Shared Public
         
          :If isHelp name
              r←'RemoveVersions filename' '' 'Remove one or more versions in the SALT file system' '' 'Modifiers:'
              r,←⊂'-version=   Specific version(s) to delete'
              r,←⊂'-collapse   Keep and renumber the LAST file if needed'
              r,←⊂'-all        Remove ALL versions'
              r,←⊂'-noprompt   do not ask for confirmation'
              r←⊃r ⋄ →0
          :EndIf
          ⎕TRAP←_TRAP
          name←fixFsep 1⊃'-version= -all -collapse -noprompt'∆parse name'1L'
          v←+/0≢¨⍙ALL ⍙VERSION
          ('You must specify ',(5×2>v)↓'only one of VERSION or ALL')⎕SIGNAL 911 if 1≠v
          fc←∊ ⍝ versions must be in the set specified
         
          :If cf←∨/'<≤≥>'=t←1↑⍙VERSION←rlb ⍙VERSION
              fc←⍎t ⋄ ⍙VERSION↓⍨←1
          :EndIf
          :If v←0∊b←'-'≠t←⍙VERSION
              fc←{(⍺≥1↑⍵)∧⍺≤1↓⍵} ⋄ ⍙VERSION←b\b/t
          :EndIf
          'too many versions supplied'⎕SIGNAL 911 if(cf∨v)∧(2-cf)≠⍴⍙VERSION←num ⍙VERSION
         
          folder←2⊃(name,'*')locateIn getSetting'workdir'
          name←2⊃FS splitLast name
          allver←{⍵[⍋⍵]}folder ListVersions name
          'Object not found'⎕SIGNAL(⍴allver)↓922 ⍝ object not there
          'NO version found to delete'⎕SIGNAL 922 if 0∊⍴⍙VERSION←(b←⍙ALL∨allver fc ⍙VERSION)/allver
         
    ⍝ Find if collapsing required
          ⍙COLLAPSE∧←¯1↑b
         
⍝ If collapsing the last version won't do anything we ignore it:
          :If ⍙COLLAPSE∧(t←⌈/allver)=1+last←⌈/0,allver~⍙VERSION
              ⍙COLLAPSE←0 ⋄ ⍙VERSION~←last←t
          :EndIf
          r←Forget folder name ⍙VERSION ⍙COLLAPSE last ⍙NOPROMPT
        ∇

        ∇ r←command merge arguments;b;na;pos;t
⍝ Put arguments in command string where %n is specified
          na←⍴t←,⊂⍣(1≡≡t)+t←,arguments
          arguments←t{⍵,⍺,⍵}¨⊂WIN⍴'"'
          :If na=+/t←'%'=command ⍝ any % specified?
⍝ We accept simple % (no n after) or %⍳na (they must ALL be there)
              pos←(¯1⌽t)/command
          :AndIf ∨/b b←(na,0)=⍴pos~na↑1↓⎕D
              (t/command)←arguments[⍋⍋b×⎕D⍳pos]
              r←∊(b⍲¯1⌽t)/command
          :Else
              r←1↓⍕command arguments
          :EndIf
        ∇

        ∇ r←Compare name;cmd;ext;files;fmt;folder;i;isns;isvar;list;max;n;names;namex;new;nf;nsi;old;o1;s;text1;text2;tmp;v;wsver;⍙PERMANENT;⍙SYMBOLS;⍙TRIM;⍙VERSION;⍙USING;⍙ZONE;⎕TRAP;cs;compare;xml;object
          :Access Shared Public
         
          :If isHelp name
              r←'Compare file1 [file2]' '' 'Compares 2 scripts or 2 versions of a script' '' 'Modifiers:'
              r,←⊂'-version=n       compare version n to current (ws=workspace version)'
              r,←⊂'-version=n1,n2   compare version n1 to n2 (if n<0 means max-n)'
              r,←⊂'-using=          to specify the EXE to use'
              r,←⊂'-permanent       to remember the EXE to use in the future'
              r,←'' 'If no external program is used the following modifiers are accepted:'
              r,←⊂'-window=         the number of lines to show before and after differences (default 2)'
              r,←⊂'-trim            trim ends prior to compare'
              r,←⊂'-symbols=        use these 2 symbols for delete and insert'
              r←⊃r ⋄ →0
          :EndIf
         
          ⎕TRAP←_TRAP ⋄ r←0 0⍴''
          →0 if 0∊⍴names←'-version= -using= -permanent -window= -trim -symbols='∆parse name'2SL'
          names←fixFsep¨names
          ⍙USING ∆default←getSetting'compare'
          wsver←'ws '≡3↑v←rlb lCase⍕⍙VERSION ⍝ there may be a version AFTER 'ws'
          ⍙VERSION←wsver{⍺:(2∊⍴,⍵)↓⍵ ⋄ ⍵}⍙VERSION ∆default 0
         
⍝ If we specify -version=ws we can also specify an object name instead of a filename
⍝ or prepend a dot before the name. In that case we use its sourcefile (experimental).
          :If wsver∨nf←'.'=1↑s←1⊃names             ⍝ is this the name of the object?
              wsver>←nf ⋄ s←nf↓s
              nsi←CRef
              :If 9=nsi.⎕NC s,'.SALT_Data'       ⍝ is it a SALTed space?
                  object←v←nsi⍎s
                  names←,⊂v.SALT_Data.SourceFile ⍝ this is its source file
              :ElseIf ~0∊⍴nf←fnData(nsi.⎕NR s)s  ⍝ is it a SALTed fn?
                  names←,⊂nf.SourceFile
              :ElseIf ~0∊⍴nf←varData⊂nsi s       ⍝ or a SALTed var?
                  xml←nf.Format≡'xml' ⋄ names←,⊂nf.SourceFile
              :EndIf                             ⍝ otherwise must be a filename
          :EndIf
          'too many files/versions'⎕SIGNAL 911 if∨/(2-wsver)<(⍴names),⍴,⍙VERSION
         
          :If ⍙PERMANENT
              'compare'saveSettings ⍙USING
          :EndIf ⍝ replace in registry
         
          nf←⍴files←names
          fmt←{folder,FS,name,((0<⍵)/'.',⍕⍵),'.',ext}
         
    ⍝ Find the file(s) and its versions
          :For i :In ⍳nf
              (folder name)←FS splitLast i⊃names ⋄ (name v ext)←splitName name
              namex←name,'.',ext ⍝ remove verno, if any
              :If i=1
                  o1←name
              :EndIf
              folder←(getSetting'workdir')ClassFolder¨⊂folder
              n←1⍳⍨×,⊃⍴¨v←folder ListVersions¨⊂namex ⍝ look in each folder
              (namex,' not found')⎕SIGNAL 22 if n>⍴v
              (folder v)←n⊃¨folder v ⋄ max←⌈/v←{⍵[⍋⍵]},v
              :If 1=nf  ⍝ only 1 file specified?
                 ⍝ 0, 1 or 2 version #s may have been specified; 0 means the last version.
                 ⍝ If 0 or 1 we assume comparison is to be made between its predecessor and V
                  (old new)←v{2∊⍴⍵:⍵ ⋄ ¯2↑(v⍳⍵)↑v}max{⍵+⍺×⍵≤0}⍙VERSION
                  ('Version not found for ',namex)⎕SIGNAL(∧/(wsver↓old,new)∊v)↓22
                  ('Versions are the same')⎕SIGNAL 911 if wsver<old=new
                  files←fmt¨old new
              :Else
                  new←max{⍵+⍺×⍵≤0}i⊃2⍴⍙VERSION
                  ('Version not found for ',namex)⎕SIGNAL(new∊v)↓22
                  files[i]←⊂fmt new
              :EndIf
          :EndFor
         
          :If (wsver∨'apl'≡lCase ⍙USING)∨' '∧.=⍙USING
              :If 9≠⎕NC(cs←'⎕se.Dyalog'),'.compare'
                  cs ⎕NS''
                  Load'[SALT]/tools/code/compare -target=',cs
              :EndIf
              compare←⎕SE.Dyalog.compare
              compare.ZONE←⍙WINDOW ∆default 2 ⋄ compare.DELINS←2↑'→',⍨' '~⍨⍙SYMBOLS ∆default'-+'
              text2←splitOnNL 2⊃files ⋄ name←⍴isns←isvar←0
              :If wsver
⍝ NOTE: the comparison is reversed for objects in the ws:
⍝ we compare the ws version TO the one on file
                  :If isvar←isns←':⍝ '∨.=1↑n←rlb 1⊃text2 ⍝ is this a namespace (as opposed to a fn)?
                      :If 0=⎕NC'object' ⍝ we were given a filename as arg and object is not defined
                          object←nsi⍎{t↑⍨¯1+⌊/':⍝'⍳⍨t←t↓⍨' '⍳⍨t←t⊃⍨1⍳⍨':'=∊1↑¨t←rlb¨⍵}text2
                      :EndIf
                      text1←⎕SRC object ⋄ name←⍕DF object
                  :ElseIf isvar←'⌷'=1↑n ⍝ var?
                      :If 0=⎕NC'xml'
                          xml←{1≡≡⍵:0 ⋄ 1∊⍴⍵:0 ⋄ '<'∊1↑2⊃⍵}text2
                      :EndIf
                      text1←name xml ∆VCR nsi⍎name←1↓(¯1+n⍳'←')↑n
                      :If xml
                          text1←(⎕UCS 10)split⍨text1~CR
                      :Else
                          text1←,⊂text1
                      :EndIf
                  :Else
⍝ We must find the name of the fn to compare to
                      'tmp'⎕NS'' ⋄ name←tmp.⎕FX text2
                      text1←remTag nsi.⎕NR name
                  :EndIf
              :Else
                  text1←splitOnNL o1←1⊃files
              :EndIf
              :If ⍙TRIM
                  (text1 text2)←rlb∘rtb¨¨text1 text2
              :EndIf
              r←(wsver+1)⊃o1(('function' 'variable' 'space'⊃⍨isvar+isns+1),' <',(rtb name),'> in the ws')
              r←'Comparing ',r,CR,'     with ',(wsver/'the one in '),2⊃files
              r←r,,CR,{0∊⍴⍵:'(they are the same)' ⋄ ⍵}text1 compare.compecv text2
          :Else
              cmd←{(⍴⍵)>p←⍵⍳']':(p↑⍵)ClassFolder p↓⍵ ⋄ ⍵}⍙USING
        ⍝ If the command contains % file placeholder use them
              cmd←cmd merge files
              ⎕CMD cmd'Normal'
          :EndIf
     ⍝ End Compare
        ∇

⍝ List command
⍝ Specs:
⍝ List [\root]path[\filter]
⍝ If a root is supplied we use that instead of the current workdir setting
⍝ If a filter is given then path is a folder and the filter is used for filenames
⍝ and any recursive call will include the same filter
⍝ If no filter is given then path may mean a folder with filter * or a folder followed by a filename

        ∇ r←List arg;b;d;dirs;es;EXT;f;files;filter;folder;f0;hasver;i;isfilter;lc;ls;m;recur;remext;rpf;t;tie;tiedalready;ver;wf;⍙EXTENSION;⍙FOLDERS;⍙FULL;⍙RAW;⍙RECURSIVE;⍙TYPE;⍙VERSIONS;⎕TRAP
          :Access Shared Public
         
          :If isHelp arg
              r←'list [path]' '' 'By default show all .dyalog files found in a path (default current workdir)' '' 'Modifiers:'
              r,←⊂'-full[=1|2]     =1 (or no value) shows full pathnames below first folder found or below ROOT'
              r,←⊂'                =2 returns full "rooted" names.'
              r,←⊂'-recursive      Recurse through folders (implies -full)'
              r,←⊂'-versions       List versions'
              r,←⊂'-folders        Only list folders'
              r,←⊂'-raw            Return unformatted date and version numbers'
              r,←⊂'-type           Show the type for functions and spaces (slower)'
              r,←⊂'-extension[=]   Specify which extension to look for (all=*) instead of .dyalog'
              r←⊃r ⋄ →0
          :EndIf
         
          ls←{⍵≡'*':'a'Dir ⍺,FS,⍵ ⋄ ('ad'Dir ⍺,FS,'*'),¨'af'Dir ⍺,FS,⍵,EXT}  ⍝ list fn
          lc←lCase⍣WIN
         
          :If recur←2=≡arg
              (folder filter ⍙FULL ⍙RECURSIVE ⍙VERSIONS ⍙RAW ⍙FOLDERS ⍙TYPE EXT)←arg ⍝ Internal call
              files←folder ls filter
          :Else
⍝ Initial call goes thru here. We accept (folders\)*(name∣filter)
              ⎕TRAP←_TRAP ⍝ ⍕ on next line is to ensure character otherwise could be ⍬
              f←⍕fixFsep 1⊃1↑'-full[∊]12 -recursive -versions -folders -raw -type -extension[=]'∆parse arg'1SL'
              ⍙FULL←num ⍙FULL
              :If FS=¯1↑FS,rpf←remExt f←(-FS=¯1↑f)↓f ⍝ drop last \ if any and check name not empty
                  rpf←f ⍝ this is a name like ".../folder/.svn" starting with a dot
              :EndIf
              (folder filter)←FS splitLast rpf
         
         ⍝ Which EXTension to use
              EXT←lc{⍵↓⍨=/2⍴⍵}'.',(0 1⍳⊂t)⊃SALTEXT'*'(t←⍙EXTENSION)
         
⍝ The arg can be 1:/.../dir (all is listed), 2:/.../name (name only) 3:/.../nam* (all nam... only)
⍝ The easiest way to do it is to list the folder and look for filter as a dir
         
              isfilter←∨/'?*'∊filter ⋄ wf←getSetting'workdir'
              :If ~isRelPath rpf ⍝ rooted?
                  wf←⊂folder ⋄ filter←rpf←(1+⍴folder)↓rpf
              :EndIf
⍝ If a filter is supplied we know the path is in 'folder'. If not we need to figure out
⍝ if 'filter' is a filename (in which case we're all set to proceed) or another folder
⍝ in which case we need to fetch all the files in it.
              (f0 folder files)←(rpf,'*')locateIn wf
⍝ If this was a folder we need to call Dir once more to get the files
              t←lc¨4⊃files                ⍝ caseless for Windows
              :If 1∊b←t∊⊂lc filter        ⍝ our name is there?
              :AndIf (b⍳1)⊃1⊃files        ⍝ and it's a folder?
                  t←(0<⍴filter)/FS,filter
                  files←'a'Dir folder,t,FS,filter←'*'
                  folder,←(~isfilter)/t
              :EndIf
          :EndIf
         
          folder,←FS ⋄ es←⊂''
          d←⎕IO⊃files ⍝ dir, ts, size, name
          dirs←(⊂'<DIR>'),(es,d⌿⊃[1]files)[;5 1 1 3]
          dirs[;2]←folder∘,¨dirs[;2]
          dirs←↓dirs
          filter,←(0∊⍴filter)/'*'
          :If ⍙RECURSIVE∧0<t←⍴dirs ⍝ do NOT enter if t=0
              r←⍬ ⋄ ⍙FULL⌈←1
              :For i :In ⍳t
                  r←r,dirs[i],List(i 2⊃dirs)filter ⍙FULL ⍙RECURSIVE ⍙VERSIONS ⍙RAW ⍙FOLDERS ⍙TYPE EXT
              :EndFor
              dirs←r
          :EndIf
          files←(es,(⍙FOLDERS⍱d)⌿⊃[1]files)[;1 5 1 4 3]
          files,←files[;2] ⍝ grab a copy of the names
          :If remext←'.*'≢EXT
              files←(EXT∘≡∘lc¨(-⍴EXT)↑¨files[;2])⌿files ⍝ keep those with the wanted extension
              files[;2]↓⍨←-⍴EXT ⍝ remove extension
          :EndIf
          :If 0<1↑⍴files
              :If remext ⍝ if the extension is kept we display all files as they are
                  files[;2 3]←⊃{(∧/b)<∧/⎕D∊⍨⍵↑⍨n←-⊥⍨b←'.'≠⍵:((n-1)↓⍵)(n↑⍵) ⋄ ⍵''}¨files[;2] ⍝ split version off name
              :EndIf
              :If 1<1↑⍴files
                  files←files[⍒⊃files[;5];]  ⍝ Desc by timestamp
                  files←files[⍋⊃files[;2];]  ⍝ Asc by name
              :EndIf
⍝ If -versions is supplied we show the versioned files wo their unversioned ones
              b←0≠∊⍴¨files[;3] ⍝ versioned files
              :If ⍙VERSIONS    ⍝ keep the versioned files for those versioned ones
                  f←b∨~t∊∪b/t←files[;2] ⍝ or those wo versions
              :Else ⍝ record the NUMBER of versions as a negative value
                  files[f/⍳⍴f;3]←{0=⍵:'' ⋄ ⍵}¨-+/¨b⊂⍨f←{⍵≢¨¯1↓0,⍵}lCase¨files[;2]
              :EndIf
              files←f⌿files ⍝ remove unwanted versions
⍝ If -type was specified we read the first few bytes of each file to figure out
⍝ what kind of file it is. We do this AFTER removing versions to minimize disk access.
              tiedalready←⎕NNUMS ⋄ files[;⍙TYPE/1]←'?' ⋄ files[;6 2]←folder∘,¨files[;6 2]
              :For f :In ⍳⍙TYPE×1↑⍴files
                  :Trap 24 25 ⍝ file tied already?
                      tie←files[f;6]⎕NTIE¨0 ⋄ t←⎕NREAD tie,83 22 0 ⋄ ⎕NUNTIE(tie∊tiedalready)↓tie
                      :If ¯17 ¯69 ¯65≡3↑t      ⍝ UTF-8 files header?
                          t←{11 92::∇ ¯1↓⍵ ⋄ 'UTF-8'⎕UCS ⍵}256|3↓t
                      :ElseIf ¯1 ¯2≡2↑t        ⍝ UCS-2 files header?
                          t←⎕UCS 163 ⎕DR 2↓t   ⍝ must be even length
                      :EndIf
                      files[f;1]←⊂'Fn'
                      :If (1↑m←2↑⎕AV[⍳5]rlb t~1⍴⎕AV)∊' ⍝:'
                          files[f;1]←⊂'Space'
                      :AndIf ':'∊1↑m ⍝ can we be more specific?
                          files[f;1]←(6⍴'If' 'Cl' 'Ns')['ICNicn'⍳m[2]]
                      :EndIf
                      :If '⌷'=1↑m
                          files[f;1]←⊂'Var'
                      :EndIf
                  :EndTrap
              :EndFor
          :EndIf
          files←0 ¯1↓files ⍝ remove real filename
         
          r←(↓files),dirs
         
          :If ~recur ⍝ finalize result
              r←⊃r
              :Select ↑⍙FULL
              :Case 1
                  r[;2]←(1+⍴specialName f0)↓¨r[;2]
              :Case 0
                  r[;2]←{2⊃FS splitLast ⍵}¨r[;2]
              :EndSelect
              :If ⍙FOLDERS
                  r←r[;,2] ⍝ when displaying folders we only get 1 column and no header
              :ElseIf ~⍙RAW
                  r[;5]←↓fmtDate⊃r[;5]
                  r[;3]←fmtVersion¨r[;3]
                  r←'Type' 'Name'('Version',⍙VERSIONS↓'s')'Size' 'Last Update'⍪r
              :EndIf
          :EndIf
     ⍝ End List
        ∇

        ∇ r←PrepareClass;s
          :Access shared
          r←ResetSettings''
⍝ Check the files needed: SALT (OK), SALTUtils (OK) and Parser + Utils:
          :If 9≠⎕SE.⎕NC s←'Parser'
              (BootPath'')BootLib s
          :EndIf
          :If 9≠⎕SE.⎕NC s←'Dyalog.Utils'
              (BootPath'')BootLib s
          :EndIf
        ∇

        ∇ prev←ResetSettings arg;i;s;⎕TRAP;lineof;special;sd;wd
          :Access shared
          prev←0 2⍴''
          ⎕TRAP←0 'E' '→0' ⍝ in case SALTUtils not included
          lineof←SettingsTable[;1]∘⍳
          'unknown setting'⎕SIGNAL 911 if 0∊⍴i←i/⍳⍴i←(0∊⍴arg)∨(⍳1↑⍴SettingsTable)=lineof⊂arg←arg~' '
          prev←SettingsTable[i;1 5]
          special←i∊lineof'workdir' 'cmddir'
          SettingsTable[i;5]←special{specialName⍣⍺⊢⍵}¨regSetting¨↓SettingsTable[i;3 4],0 ⍝ cannot be empty
          '⎕se.Dyalog.Callbacks'⎕NS'' ⍝ make sure it is there
     ⍝ We reset these even if they were not requested to be
          ⎕SE.SALTUtils.ConfirmEdit←∨/'1yY'∊5⊃SettingsTable[lineof⊂'edprompt';]
          ⎕SE.SALTUtils.DEBUG←¯1+'01'⍳⍬⍴5⊃SettingsTable[lineof⊂'debug';]
          ⎕SE.SALTUtils.SETTS←∨/' atinfo '⍷s←1⌽'  ',5⊃SettingsTable[lineof⊂'track';]
          :If ∨/' new '⍷s
              wd←5⊃SettingsTable[lineof⊂'workdir';]
              :If ((⍴sd)↑wd)≢sd←getEnvir'SALT' ⍝ find folders; we accept the "old" OS delimiters for backwards compatibility
                  ⎕SE.SALTUtils.NewObjectsFolder←wd[⍳¯1+⌊/wd⍳PATHDEL] ⍝ 1st dir is the one we use
              :Else
                  (5⊃SettingsTable[lineof⊂'track';])←¯1↓1↓'new'⎕R''⊢s
              :EndIf
          :EndIf
        ∇

        PrepareClass  ⍝ touch-up

        ∇ r←getSetting s;b
          r←5⊃SettingsTable[SettingsTable[;1]⍳⊂s;]
          :If ∨/b←'track' 'cmddir' 'workdir'∊⊂s
              r←r splitOn(-1↑b)↓PATHDEL  ⍝ split track on ∘ only
          :EndIf
        ∇

        ∇ r←Settings arg;add;empty;i;k;names;new;old;pairs;rem;set;val;valid;vars;⍙PERMANENT;⍙PREPEND;⍙REMOVE;⍙RESET;⎕TRAP;cw;⎕IO;prevval;msg;ACN
          :Access Public Shared
          ⎕IO←1
          :If isHelp arg
              r←'Settings [Name [Value]]' '' 'Set and returns the [new] settings' '' 'Modifiers:'
              r,←⊂'-permanent      sets  permanently'
              r,←⊂'-reset          loads permanently saved values'
              r,←'' 'The possible settings are:' ''
              r,←↓⍕SettingsTable[;1 2]
              r←⊃r ⋄ →0
          :EndIf
          ⎕TRAP←_TRAP
          pairs←{(1⌈⍴⍵)↑⍵}'-permanent -reset'∆parse arg'2SL'
          names←SettingsTable[;1]
          valid←0<i←names strIndex pairs[1]←lCase¨pairs[1]
          'Invalid setting'⎕SIGNAL 911 if valid⍱empty←0∊⍴↑pairs
          '-Permanent and -Reset are mutually exclusive'⎕SIGNAL 911 if ⍙PERMANENT∧⍙RESET
          pairs[valid/1]←names[valid/i] ⍝ restore to full length if it wasn't
          :If ⍙RESET
              r←prevval←ResetSettings 1⊃pairs ⋄ →empty/0
          :EndIf
         
          :If 1=⍴pairs
              i←(i~0),empty/⍳1↑⍴SettingsTable ⍝ all or only a specific value
              r←SettingsTable[i;1 5]   ⍝ return specific value
              :If empty⍱⍙RESET
                  prevval←2⊃,r
              :EndIf
          :Else ⍝ set it
              (set val)←pairs ⋄ old←5⊃SettingsTable[i;]
              :Select set
              :CaseList cw←'cmddir' 'workdir' ⍝ special treatment for folders; expand [ENVVARS], account for ';'s
                  new↓⍨←∨/(add rem)←',~'=1↑new←val
            ⍝ maybe use: add←⍙PREPEND ⋄ rem←⍙REMOVE ⋄ new←val
            ⍝ Perform minimal validation on paths
                  new←fixBRname new  ⍝ look for [NAMES] and replace them
                  'invalid path specified (cannot contain [ or ])'⎕SIGNAL 922 if∨/'[]'∊new
         
                  val←{⍵/⍨' '∨.≠¨⍵}new splitOn PATHDEL    ⍝ split names that need to be and remove empties
                  :If add
                      val←∪val,old splitOn PATHDEL        ⍝ side-effect: new path moved ahead
                  :ElseIf rem
                      val←val~⍨old splitOn PATHDEL
                  :EndIf
                  val←val,(0∊⍴val)/SettingsTable[i;4]
         
            ⍝ The new workdir cannot be SALT if we are tracking new objects
                  :If cw[2]≡⊂set
                  :AndIf (⊂'new')∊getSetting'track'
                      msg←'SALT workdir cannot be set while tracking new objects'
                      msg ⎕SIGNAL 911 if k≡(⍴k←getEnvir'SALT')↑new
                  :EndIf
                  val←↑{⍺,'∘',⍵}/val ⍝ merge paths USING JOT (∘), NOT ; or :, this is SALT's delimiter not the OS'
              :Case 'newcmd'
                  'new commands must be detected AUTO[matically] or MANUAL[ly]'⎕SIGNAL 911↓⍨∨/'auto' 'manual'≡¨⊂val←lCase val
              :Case 'edprompt'
                  val←⍕⎕SE.SALTUtils.ConfirmEdit←∨/'1yY'∊val
              :Case 'debug'
                  val←⍕⎕SE.SALTUtils.DEBUG←{∨/'yY'∊⍵:1 ⋄ +/2⊃⎕VFI ⍵}val
              :Case 'mapprimitives'
                  val←⍕∨/'1yY'∊val
              :Case 'varfmt'
                  'Variables storing format can only be APL or XML'⎕SIGNAL 911↓⍨∨/'apl' 'xml'≡¨⊂val←lCase val
              :Case 'track'
                  new←new↓⍨∨/(add rem)←',~'=↑new←lCase val
                  val←{⍵/⍨' '∨.≠¨⍵}new splitOn' ,∘' ⍝ split features that need to be and remove empties
                  ACN←'atinfo' 'compiled' 'new'     ⍝ the order is important
                  msg←'. Choose from ',↑{⍺,', ',⍵}/ACN
                  msg←msg,⍨'Invalid track setting: ',¯1↓⍕val/⍨k←~val∊ACN
                  msg ⎕SIGNAL 911 if∨/k
                  :If add
                      val←∪val,old splitOn'∘'       ⍝ side-effect: moved ahead
                  :ElseIf rem
                      val←val~⍨old splitOn'∘'
                  :EndIf
                  'TS tracking not available in this version'⎕SIGNAL 911 if FTS<val∊1↑ACN
         
             ⍝ Check that 'new' has a valid working folder to work with
                  :If (⊂'new')∊val
                      ⎕SE.SALTUtils.NewObjectsFolder←''
                      msg←'The current workdir is SALT and new objects cannot be stored there, change it first'
                      msg ⎕SIGNAL 911 if(⊂set←1⊃getSetting'workdir')∊(getEnvir'SALT')'[SALT]'
                      ⎕SE.SALTUtils.NewObjectsFolder←set
                  :EndIf
                  ⎕SE.SALTUtils.(SETTS SETCOMPILED)←ACN[⍳2]∊val
                  val←↑{⍺,'∘',⍵}/val,(0∊⍴val)/⊂'' ⍝ merge all
              :EndSelect
              prevval←i⊃SettingsTable[;5]
              SettingsTable[i;5]←⊂val
              r←SettingsTable[i;1 5]
          :EndIf
         
          :If ⍙PERMANENT
              saveSettings/r
          :EndIf
          :If ~empty
              r←prevval ⍝ return previous value
          :EndIf
     ⍝ End Settings
        ∇

        ∇ r←Clean Args;t;ref;⍙DELETEFILES
    ⍝ Remove SALT tags from the objects
          :Access Public Shared
          :If isHelp Args
              r←'Clean [namespace]' '' 'Cleanup namespace by removing all SALT tags in objects recursively.'
              r,←'Default is current namespace' '' 'Modifiers:' '-deletefile(s)  remove also files associated with tagged objects'
              r←⊃r ⋄ →0
          :EndIf
         
          ref←CRef
          :If 9∊ref.⎕NC t←⍕'-deletefile(s)'∆parse Args'1S'
              ref←ref⍎t
          :EndIf
          r←⍕⍙DELETEFILES cleanWS ref           ⍝ assume current space
          r←'* Cleanup done, ',r,' tags removed',⍙DELETEFILES/' and associated files'
        ∇

⍝ $Revision: 27928 $
    :EndClass

    :Class UnicodeFile
⍝ Version 12 compatible version

        ⎕ML ⎕IO←0 1
        ⎕avu[4]←13      ⍝ make sure CR is here

⍝==============================================================================⍝
⍝     Declaration of Variables                                                 ⍝
⍝     (commented ones are defined later in the file )                          ⍝
⍝==============================================================================⍝

  ⍝ Public Variables :
    ⍝ :Property Public NewLine          ⍝ Vector of Unicode Scalar representing the New Line sequence (empty if auto-detected)
    ⍝ :Property Public Raw              ⍝ Unicode scalars of the file
    ⍝ :Property Public Text             ⍝ Encapsulation of Raw in APL text through the translate table
    ⍝ :Property Public NestedText       ⍝ Encapsulation of NestedText
    ⍝ :Property Public Encoding         ⍝ A string to get or set the current unicode encoding
    ⍝ :Property Public Errors           ⍝ Friendly access to error state (write anything = reset)
        :Field Public Malformed←⍬           ⍝ indices of malformed characters in Raw
        :Field Public Illegal←⍬             ⍝ indices of illegal characters in Raw
        :Field Public Untranslatable←⍬      ⍝ indices of untranslatable characters in Text
        :Field Public Shared UnicodeEN←500  ⍝ ⎕EN of the unicode errors when using the public shared Read/Write functions

  ⍝ Private counterparts of public variables :
        :Field Private _Raw←⍬               ⍝ Data storage for property Raw
        :Field Private _Replacement←'?'     ⍝ Data storage for property Replacement

  ⍝ Private Fields for Errors :
    ⍝ :Field Private _ErrorMessages     ⍝ List of error messages      - See the end
        :Field Private _Errors←⍬            ⍝ Data storage for property Errors

  ⍝ Private Fields for New Lines :
        :Field Private _NewLine←⍬           ⍝ New Line sequence currently in use (will be auto-detected if empty)
        :Field Private _NewLineUser←⍬       ⍝ User-defined new line sequence
        :Field Private _NewLines←,¨(13 10)10 13 133    ⍝ Accepted new line sequences for autodetection

  ⍝ Private Fields for Encoding - See the end
    ⍝ :Field Private _TableBOM
    ⍝ :Field Private _Encoding
    ⍝ :Field Private _EncodingDefault
    ⍝ :Field Private _EncodingUser
    ⍝ :Property Private _EncodingName
    ⍝ :Property Private _EncodingList
    ⍝ :Property Private _Width
    ⍝ :Property Private _Endian
    ⍝ :Property Private _BOM
    ⍝ :Property Private _BOMlist

  ⍝ Misc :
        :Field Private _FileName←''         ⍝ file path
        :Field Private V11←'11.0.'≡5↑2⊃'.' ⎕wg 'APLVersion'


⍝==============================================================================⍝
⍝     Public Methods of Shared Use                                             ⍝
⍝==============================================================================⍝

        ∇ r←ReadRaw arg;u
          :Access Public Shared
    ⍝ ReadRaw 'filename' {'format'}
          :Trap 0                 ⍝ forward errors to user
              r←1 Read arg
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN
          :EndTrap
        ∇

        ∇ r←ReadText arg;u;err
          :Access Public Shared
    ⍝ ReadText 'filename' {'format'}
          :Trap 0                 ⍝ forward errors to user
              r←2 Read arg
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN
          :EndTrap
        ∇

        ∇ r←ReadNestedText arg;u
          :Access Public Shared
    ⍝ ReadNestedText 'filename' {'format'}
          :Trap 0                 ⍝ forward errors to user
              r←3 Read arg
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN
          :EndTrap
        ∇

        ∇ r←type Read arg;u;err
          :Access Private Shared
    ⍝ (type∊1 2 3) Read 'filename' {'format'}
          u←⎕NEW ⎕THIS arg
          :Select type
          :Case 1
              r←u.Raw
          :Case 2
              r←u.Text
          :Case 3
              r←u.NestedText
          :Else
              r←⍬
          :EndSelect
          :If 1=2|⊃u.Errors             ⍝ special case when file does not exist
              'File not found'⎕SIGNAL 22
          :ElseIf 0 4∧.≠1⊃u.Errors      ⍝ signal any error excepted when only unmarked file (correctly read)
              ('UNICODE ERRORS: ',2⊃u.Errors)⎕SIGNAL UnicodeEN
          :EndIf
        ∇

        ∇ Write arg;data;u;msg
          :Access Public Shared
    ⍝ Write 'filename' data {'format'}
          msg←'Usage = Write ''filename'' (data) {''encoding''} {(new line sequence)}'
          :If ~(⊃⍴arg←,arg)∊2 3 4
              ('LENGTH ERROR: ',msg)⎕SIGNAL 5           ⍝ length error
          :EndIf
          data←2⊃arg ⋄ arg←(1↑arg),(2↓arg)  ⍝ separate data from other arguments
         
          :Trap 0
              :Trap 11                      ⍝ change the usage message if it pops up
                  u←⎕NEW ⎕THIS arg
                  {}u.Text                  ⍝ trigger auto-detection of NewLine sequence
              :Else
                  ('DOMAIN ERROR: ',msg)⎕SIGNAL 11
              :EndTrap
              u.Errors←0                ⍝ reset errors (we don't care whether reading was OK or not)
              :If istext data           ⍝ Text
                  u.Text←data
              :ElseIf 1 3∨.=10|⎕DR data ⍝ Integers (can be boolean)
                  u.Raw←data
              :ElseIf ∧/istext¨data     ⍝ Nested Text
                  u.NestedText←data
              :Else
                  msg←'DOMAIN ERROR: Data (the second element) must be either '
                  msg,←'Raw (integer vector), Text (character vector), or Nested Text (vector of character vectors)'
                  msg ⎕SIGNAL 11        ⍝ domain error
              :EndIf
              :If 0≠1⊃u.Errors          ⍝ check for unicode errors
                  ('UNICODE ERRORS: ',2⊃u.Errors)⎕SIGNAL UnicodeEN
              :EndIf
              u.Export                  ⍝ write if no error
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN
          :EndTrap
        ∇

⍝==============================================================================⍝
⍝     Public Methods of Instance Use                                           ⍝
⍝==============================================================================⍝

        ∇ Make arg;filename;format;msg;i;t
          :Access Public
          :Implements Constructor
          msg←'Arguments should be : ''filename'' {''encoding''} {(new line sequence)}'
          ('DOMAIN ERROR: ',msg)⎕SIGNAL(~istext⊃arg)/11     ⍝ check filename is text
          Encoding NewLine←''⍬         ⍝ initialize to default
          :Trap 0
              :Select |≡arg←,arg
              :Case 1
                  _FileName←arg
              :Case 2
                  :Select ⊃⍴arg
                  :Case 1
                      _FileName←(⊃arg)
                  :CaseList 2 3
                      _FileName←⊃arg
                      i←⍋t←2|⎕DR¨arg←1↓arg    ⍝ types of arguments
                      :Select t[i]
                      :Case ,0          ⍝ text : enconding
                          Encoding←⊃arg
                      :Case ,1          ⍝ numbers : new line sequence
                          NewLine←⊃arg
                      :Case 0 1         ⍝ both
                          Encoding NewLine←arg[i]
                      :Else
                          ('DOMAIN ERROR: ',msg)⎕SIGNAL 11
                      :EndSelect
                  :Else
                      ('LENGTH ERROR: ',msg)⎕SIGNAL 5
                  :EndSelect
              :Else
                  ('DOMAIN ERROR: ',msg)⎕SIGNAL 11
              :EndSelect
              Import                ⍝ try to import the unicode file
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN     ⍝ file i/o errors
          :EndTrap
        ∇

        ∇ Import;bytestream
          :Access Public
    ⍝ refresh the data from the file
          :Trap 0
          ⍝ first, try to read bytestream from the file
              bytestream←readfile _FileName
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN     ⍝ file i/o error
          :EndTrap
          :If ~ERR_EXIST∊_Errors
          ⍝ then process the bytestream into unicode scalars
              Raw←decode gather removeBOM getencoding bytestream
          :Else
              Raw←⍬
          :EndIf
        ∇

        ∇ Export;bytestream
          :Access Public
    ⍝ refresh the file with the data
    ⍝ be sure to have check for errors before doing this !
          bytestream←addBOM split encode Raw
          :Trap 0
              _FileName writefile bytestream
          :Else
              (⊃⎕DM)⎕SIGNAL ⎕EN     ⍝ file i/o error
          :EndTrap
        ∇



⍝==============================================================================⍝
⍝     Public Properties                                                        ⍝
⍝==============================================================================⍝

        :Property Raw           ⍝ legalized Vector of Unicode scalars
        :Access Public
            ∇ r←Get
              r←_Raw
            ∇
            ∇ set args;bytestream;msg;data
              data←,args.NewValue
              msg←'DOMAIN ERROR: Raw data must be an integer vector'
              msg ⎕SIGNAL(1 3∧.≠10|⎕DR data)/11
             
              :If 'Import'≢2⊃2↑⎕SI     ⍝ if writing Raw directly, clear Import errors
                  _Errors~←ERR_BADBOM∪ERR_NOBOM∪ERR_MALFORMED
              :EndIf
              :If 'set_Text'≢2⊃2↑⎕SI    ⍝ if writing Raw directly, clear text errors
                  _Errors~←ERR_NL∪ERR_UNTRANS
              :EndIf
              _Raw←legalize data
            ∇
        :EndProperty

        :Property Text          ⍝ APL Text Vector
        :Access Public
            ∇ r←Get
              r←unicode2apl 1 newlines Raw
            ∇
            ∇ Set args;msg;data
              data←,args.NewValue
              msg←'DOMAIN ERROR: Text must be an character vector'
              msg ⎕SIGNAL(istext data)↓11
              Raw←0 newlines apl2unicode data
            ∇
        :EndProperty

        :Property NestedText    ⍝ APL Nested Text Vector (no newline characters)
        :Access Public
            ∇ r←get;NL;txt
              txt←Text                          ⍝ read text and detect NewLine
              :If ~V11
              :OrIf _NewLine∧.∊⊂_Unicode        ⍝ mappable new line
                  NL←,unicode2apl _NewLine
              :Else
                  NL←,⎕AV[4]                    ⍝ un-mappable new line
              :EndIf
              r←(⍴NL)↓¨(NL⍷txt)⊂txt←NL,txt
            ∇
            ∇ Set args;data;msg;NL
              data←,args.NewValue
              msg←'DOMAIN ERROR: NestedText must be an vector of character vectors'
              msg ⎕SIGNAL(∧/istext¨data)↓11
              :If 0=⍴,_NewLine
                  _NewLine←,⊃_NewLines          ⍝ default new line
              :EndIf
              :If ~V11
              :OrIf _NewLine∧.∊⊂_Unicode        ⍝ mappable new line
                  NL←,unicode2apl _NewLine
              :Else
                  NL←,⎕AV[4]                    ⍝ un-mappable new line
              :EndIf
              Text←(-⍴NL)↓⊃data,.,⊂NL
            ∇
        :EndProperty

        :Property Encoding      ⍝ string to get or set the current encoding of the file
        :Access Public
            ∇ r←Get
              r←¯1↓⊃_EncodingName,.,'-'
            ∇
            ∇ Set args;name;names;charset;bool;msg
              name←,args.NewValue
              msg←'DOMAIN ERROR: Encoding must be a character vector'
              msg ⎕SIGNAL(istext name)↓11               ⍝ Must be char
              :If 0=⍴name
                  _EncodingUser←⍬
                  _Encoding←_EncodingDefault
              :Else
                  charset←'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 abcdefghijklmnopqrstuvwxyz0123456789'
                  name←' ',charset[1+37|¯1+charset⍳name]    ⍝ go uppercase and replace anything not in charset by spaces
                  name←1↓¨(' '=name)⊂name                   ⍝ partition on spaces
                  name←(0≠(⊃∘⍴)¨name)/name                  ⍝ remove empty partitions
                  bool←(⊂name){(,¨⍺)≡(,¨⍵)}¨_EncodingList   ⍝ names that match
                  :If ∨/bool
                      _Encoding←_EncodingUser←bool⍳1        ⍝ first name that match
                  :Else
                      msg←'DOMAIN ERROR: Accepted Formats are : ',⊃,/{'''',(¯1↓⊃⍵,.,'-'),''' '}¨_EncodingList
                      msg ⎕SIGNAL 11                            ⍝ no match
                  :EndIf
              :EndIf
            ∇
        :EndProperty

        :Property Errors         ⍝ friendly access to error state
        :Access Public
            ∇ Set args           ⍝ write anything = reset
              _Errors←ERR_OK
            ∇
            ∇ r←Get;err;n;msg    ⍝ read as a 2-el vector : (scalar error number) (string catenation of error messages)
              err←∪_Errors
              :If 0=+/|err       ⍝ no error (empty or containing zeros)
                  r←0 ''
              :Else
                  err←err~0
                  err←err[⍋err]
                  n←2⊥(⌽⍳⍴_ErrorMessages)∊err               ⍝ base-2 encoding of error numbers
                  msg←'[',¯2↓⊃_ErrorMessages[err],.,⊂'] ['  ⍝ catenation of the messages
                  r←n msg
              :EndIf
            ∇
        :EndProperty

        :Property NewLine
        :Access Public
            ∇ Set args;data
              data←,args.NewValue
              :If 1 3∧.≠10|⎕DR data
                  'DOMAIN ERROR: NewLine sequence must be a vector of Unicode scalars'⎕SIGNAL 11
              :EndIf
              _NewLine←_NewLineUser←data
            ∇
            ∇ r←Get
              r←_NewLine
            ∇
        :EndProperty

⍝==============================================================================⍝
⍝     Private Methods                                                          ⍝
⍝==============================================================================⍝

⍝ Private Dataflow of the UnicodeFile Class :
⍝ ┌───────────────────────┬──────────────┬───────────┬──────────────┬───────────────┐
⍝ │ Layer (type of data)  │ Importing    │           │ Exporting    │ Errors        │
⍝ │                       │ process      │           │ process      │ Raised        │
⍝ ├───────────────────────┼──────────────┼───────────┼──────────────┼───────────────┤
⍝ │ File                  │              │           │              │               │
⍝ │                       │ readfile     │           │ writefile    │ ERR_EXIST     │\
⍝ │ Marked Bytestream     │    ↓         │           │    ↑         │               │ |
⍝ │                       │ getencoding  │           │    ↑         │ ERR_NOBOM,ERR_BADBOM
⍝ │                       │    ↓         → _Encoding │    ↑         │               │ |
⍝ │                       │ removeBOM    │           │ addBOM       │               │ | Import/Export
⍝ │ Bytestream            │    ↓         │           │    ↑         │               │ |
⍝ │                       │ gather       │           │ split        │ ERR_ODD       │ |
⍝ │ Coded Wide Characters │    ↓         │           │    ↑         │               │ |
⍝ │                       │ decode       │           │ encode       │ ERR_MALFORMED │/
⍝ │                    /  │    ↓         │ _Raw      ←    ↑         │               │\
⍝ │                   |   │ legalize     │           │ legalize     │ ERR_ILLEGAL   │ | Raw Property
⍝ │ Unicode Scalars   |   │    ↓         → _Raw      │    ↑         │               │/
⍝ │                   |   │ newlines     │           │ newlines     │ ERR_NL        │\
⍝ │                    \  │    ↓         → _NewLine  │    ↑         │               │ |
⍝ │                       │ unicode2apl  │           │ apl2unicode  │ ERR_UNTRANS   │ | Text Property
⍝ │ APL Text              │              → _Text     →              │               │/
⍝ └───────────────────────┴──────────────┴───────────┴──────────────┴───────────────┘
⍝ NB : All functions must accept and then return empty vectors

  ⍝ Importing Nested Text when no newline is set put ⎕AV[4] translated to 13, should be the default
  ⍝ When import text and no newline set, ⎕av[4] alone should be mapped to the default
  ⍝   :AndIf ~(~toquadav)∧(old≡,4⊃_Unicode)  ⍝ when from ⎕AV, ⎕AV[4] is accepted as homogeneous to the chosen sequence => no error (unless _NewLine∊_NewLines)

  ⍝ specific functions for autodetecting newlines,
  ⍝ getting its unicode representation and APL representation

        ∇ r←istext data
    ⍝ Is data text (V12 compatible)
          r←(10|⎕DR data)∊0 2
        ∇

        ∇ bytestream←readfile filename;fic
    ⍝ Read file as positive integers ∊ [0;255]
    ⍝ Will signal on any file i/o error excepted FILE NAME ERROR (22)
          fic←0
          bytestream←⍬
          :Trap 22      ⍝ FILE NAME ERROR = file does not exist
              fic←filename ⎕NTIE 0
              bytestream←256|⎕NREAD fic 83(⎕NSIZE fic)0
              ⎕NUNTIE fic
              _Errors~←ERR_EXIST
          :Else
              _Errors∪←ERR_EXIST
          :EndTrap
        ∇
        ∇ name writefile bytestream;fic
    ⍝ Write byte stream to file, creating it or overwriting it
    ⍝ Will signal any file i/o error
          fic←0
          :Trap 22       ⍝ try to create it
              fic←name ⎕NCREATE 0
          :Else          ⍝ else open it
              fic←name ⎕NTIE 0
          :EndTrap
          0 ⎕NRESIZE fic
          :Trap 11 ⍝ there used to be a bug where NAPPEND 83 would accept >127
              bytestream ⎕NAPPEND fic 83
          :Else
              (bytestream-256×bytestream>127)⎕NAPPEND fic 83
          :EndTrap
          ⎕NUNTIE fic
          _Errors~←ERR_EXIST
        ∇


        ∇ bytestream←getencoding bytestream;header;match
    ⍝ Reads the BOM and deduce the encoding if the user hasn't forced the encoding
          _Errors~←ERR_NOBOM∪ERR_BADBOM      ⍝ clear errors before updating them
          header←(⊃⌈/⍴¨_BOMlist)↑bytestream  ⍝ beginning of the bytestream
          match←⊃¨_BOMlist⍷¨⊂header          ⍝ BOMs that match
          match←match⍳1                      ⍝ first row of table that matches
          :If 0=⍴,_EncodingUser              ⍝ auto-detect
              :If match>⍴_BOMlist            ⍝ nothing detected : default
                  _Errors∪←ERR_NOBOM
                  _Encoding←_EncodingDefault
              :Else                          ⍝ BOM detected
                  _Encoding←match
              :EndIf
          :ElseIf (match≤⍴_BOMlist)∧(match≠_Encoding)  ⍝ user specified one and we detected another
              _Errors∪←ERR_BADBOM
          :EndIf
        ∇
        ∇ bytestream←removeBOM bytestream
    ⍝ Removes the BOM from the bytestream
          :If ⊃_BOM⍷bytestream
              bytestream←(⍴_BOM)↓bytestream
          :EndIf
        ∇
        ∇ bytestream←addBOM bytestream
    ⍝ Adds the BOM to the bytestream
          bytestream←_BOM,bytestream
          _Errors~←ERR_NOBOM∪ERR_BADBOM
        ∇

        ∇ wchars←gather bytestream;width;length;encoding
    ⍝ Depending on the width of the encoding, gather bytes into wide characters
          :If _Width>1
              length←(⍴bytestream)÷_Width
              :If length≠⌊length
                  length←⌊length                       ⍝ ignore the last word if it is not complete
                  _Errors∪←ERR_ODD                     ⍝ indicate file size is odd
              :Else
                  _Errors~←ERR_ODD
              :EndIf
              bytestream←⍉(length,_Width)⍴bytestream
              :If _Endian=1                            ⍝ swap only if little endian
                  bytestream←⊖bytestream
              :EndIf
              wchars←256⊥bytestream
          :Else
              wchars←bytestream
          :EndIf
        ∇
        ∇ bytestream←split wchars;width;length;encoding
    ⍝ Depending on the width of the encoding, split wide characters into bytes
          _Errors~←ERR_ODD
          :If _Width>1
              bytestream←(_Width⍴256)⊤wchars   ⍝ split wchars into bytes
              :If _Endian=1                    ⍝ swap only if little endian
                  bytestream←⊖bytestream
              :EndIf
              bytestream←,⍉bytestream
          :Else
              bytestream←wchars
          :EndIf
        ∇

        ∇ codes←decode wchars
    ⍝ Depending of the encoding, decodes the wide characters into Unicode scalars
          codes←wchars   ⍝ by default: empty or other encoding
          :If 0≠⍴,wchars
              :Select Encoding
              :Case 'UTF-8'
                  codes←readutf8 wchars
              :CaseList 'UTF-16-LE' 'UTF-16-BE'
                  codes←readutf16 wchars
              :EndSelect
          :EndIf
        ∇
        ∇ wchars←encode codes
    ⍝ Depending on the encoding, encode the Unicode scalars into wide characters
          wchars←codes   ⍝ by default: empty or other encoding
          :If 0≠⍴,wchars
              :Select Encoding
              :Case 'UTF-8'
                  wchars←writeutf8 codes
              :CaseList 'UTF-16-LE' 'UTF-16-BE'
                  wchars←writeutf16 codes
              :EndSelect
          :EndIf
        ∇

        ∇ unicode←rdU8 wchars;class;⎕IO;sect;cut;bad;touni;c0;c1;seq;bseq;tmp;todo
          :Access public shared
    ⍝ Get Unicode scalars from UTF-8 characters,
    ⍝ replacing malformed characters by U+FFFD and raising the malformed error flag.
    ⍝ Don't forget to use the legalize function on the output after reading utf
    ⍝          Binary Scalar Value         │                          UTF8 byte sequences [class of byte]                            │
    ⍝                                      │ 1st byte     │ 2nd byte     │ 3rd byte     │ 4th byte     │ 5th byte     │ 6th byte     │
    ⍝ ─────────────────────────────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┼──────────────┤
    ⍝  00000000 00000000 00000000 0xxxxxxx │ 0xxxxxxx [0] │              │              │              │              │              │
    ⍝  00000000 00000000 00000yyy yyxxxxxx │ 110yyyyy [2] │ 10xxxxxx [1] │              │              │              │              │
    ⍝  00000000 00000000 zzzzyyyy yyxxxxxx │ 1110zzzz [3] │ 10yyyyyy [1] │ 10xxxxxx [1] │              │              │              │
    ⍝  00000000 000uuuzz zzzzyyyy yyxxxxxx │ 11110uuu [4] │ 10zzzzzz [1] │ 10yyyyyy [1] │ 10xxxxxx [1] │              │              │
    ⍝  000000vv uuuuuuzz zzzzyyyy yyxxxxxx │ 111110vv [5] │ 10uuuuuu [1] │ 10zzzzzz [1] │ 10yyyyyy [1] │ 10xxxxxx [1] │              │
    ⍝  0wvvvvvv uuuuuuzz zzzzyyyy yyxxxxxx │ 1111110w [6] │ 10vvvvvv [1] │ 10uuuuuu [1] │ 10zzzzzz [1] │ 10yyyyyy [1] │ 10xxxxxx [1] │
    ⍝ - NB : in square brackets [*] is the class of the byte :
    ⍝        bytes of class 1 are called "continuation bytes", bytes of class >1 are called "start bytes"
    ⍝ - UTF-8 malformed characters must be replaced by U+FFFD. They can be :
    ⍝   ∘ a continuation byte out of a sequence (even if right after a correct sequence)
    ⍝   ∘ a sequence with not enough continuation bytes (including the case of a lonely start byte)
    ⍝   × overlong sequences (a unicode number must be embedded in the shortest possible sequence, else it is an overlong sequence)
    ⍝     NB : we don't care reading overlong sequences because we convert to unicode scalars. It is problematic only if you write UTF-8.
    ⍝   ∘ bytes equal to 0xFE or 0xFF (class 7)
          ⎕IO←0 ⋄ sect←128 64 32 16 8 4 2 2
          c1←~c0←0=class←(sect/⍳8)[wchars]     ⍝ class of each byte according to the table above
          bad←cut/⍨c0∨cut←class>1              ⍝ assume >1 sequences are wrong
          unicode←(~bad)\c0/wchars             ⍝ grab the good ones now
          :If ∨/tmp←bad                        ⍝ check the groups
              bad\←bseq←{(1↓⍵)≢1↓⍵[0]⍴1}¨(seq←c1/cut)⊂c1/class
              (bad/unicode)←65533              ⍝ and mark the string
          :AndIf ∨/todo←~bseq                  ⍝ those to do
              touni←(0,+\sect)∘{64⊥⍵-s↑⍺[s←⍴⍵],6⍴128}
              unicode+←tmp\todo\touni¨todo/seq⊂c1/wchars
          :EndIf
        ∇

        ∇ uc←readutf8 ch;bad
    ⍝ This is the private version for this class
          :If 0≠⍴Malformed←bad/⍳⍴bad←65533=uc←rdU8 ch
              _Errors∪←ERR_MALFORMED
          :Else
              _Errors~←ERR_MALFORMED
          :EndIf
        ∇

        ∇ wchars←wrU8 unicode;uni2wchars
          :Access public shared
    ⍝ Get UTF-8 characters from Unicode scalars.
    ⍝ Don't forget to use the legalize function on the input before writing utf.
      ⍝ Range of values in UCS-4        Sequence of octets in UTF-8
      ⍝ x = 0000 0000 .. 0000 007F;     x;
      ⍝ x = 0000 0080 .. 0000 07FF;     C0 + x/2∧6;    80 + x%2∧6;
      ⍝ x = 0000 0800 .. 0000 FFFF;     E0 + x/2∧12;   80 + x/2∧6%2∧6;   80 + x%2∧6;
      ⍝ x = 0001 0000 .. 001F FFFF;     F0 + x/2∧18;   80 + x/2∧12%2∧6;  80 + x/2∧6%2∧6;   80 + x%2∧6;
      ⍝ x = 0020 0000 .. 03FF FFFF;     F8 + x/2∧24;   80 + x/2∧18%2∧6;  80 + x/2∧12%2∧6;  80 + x/2∧6%2∧6;   80 + x%2∧6;
      ⍝ x = 0400 0000 .. 7FFF FFFF;     FC + x/2∧30;   80 + x/2∧24%2∧6;  80 + x/2∧18%2∧6;  80 + x/2∧12%2∧6;  80 + x/2∧6%2∧6;   80 + x%2∧6;
      ⍝ x ≥ 8000 0000                   (ISO 10646-1 does not indicate what to do)
          uni2wchars←{
              ⍵<128:⍵                                                     ⍝ 0000 0000 .. 0000 007F
              ⍵<2048:192 128+64 64⊤⍵                                      ⍝ 0000 0080 .. 0000 07FF
              ⍵<65536:224 128 128+64 64 64⊤⍵                              ⍝ 0000 0800 .. 0000 FFFF
              ⍵<2097152:240 128 128 128+64 64 64 64⊤⍵                     ⍝ 0001 0000 .. 001F FFFF
              ⍵<67108864:248 128 128 128 128+64 64 64 64 64⊤⍵             ⍝ 0020 0000 .. 03FF FFFF
              ⍵<2147483648:252 128 128 128 128 128+64 64 64 64 64 64⊤⍵    ⍝ 0400 0000 .. 7FFF FFFF
              224 128 128+64 64 64⊤65533                                  ⍝ ≥ 8000 000 : replace by U+FFFD.
          }
          wchars←⊃,/uni2wchars¨unicode
        ∇

        ∇ ch←writeutf8 uni
          ch←wrU8 uni
          _Errors~←ERR_MALFORMED ⍝ this var cannot be written by <wrU8>
        ∇


        ∇ unicode←readutf16 wchars;class;malformed;wchars2uni;illegal
    ⍝ Get Unicode scalars from UTF-16 characters,
    ⍝ replacing malformed characters by U+FFFD and raising the malformed error flag.
    ⍝ Don't forget to use the legalize function on the output after reading utf.
      ⍝   16-bit value   │ condition  │ class   │ UTF-16 rule
      ⍝ ─────────────────┼────────────┼─────────┼──────────────────────────────
      ⍝ 110110xxxxxxxxxx │ 54=⌊⍵÷1024 │ class 2 │ must be followed by a class 1
      ⍝ 110111xxxxxxxxxx │ 55=⌊⍵÷1024 │ class 1 │ must follow a class 2
      ⍝ else             │ else       │ class 0 │ no rule
          class←3|55 54⍳⌊wchars÷1024
          wchars class←(⊂(~0,¯1↓2 1⍷class))⊂¨wchars class  ⍝ gather (2 1) sequences, partition others
          malformed←~class∊(,0)(2 1)               ⍝ malformed characters
          wchars2uni←{
              ⍺:65533                              ⍝ malformed characters : put U+FFFD
              2=⍴,⍵:+/65536,1024 1×⍵-55296 56320   ⍝ surrogate pairs
              ⊃⍵                                   ⍝ single characters
          }
          unicode←malformed wchars2uni¨wchars
          Malformed←malformed/⍳⍴malformed
          :If 0≠⍴Malformed
              _Errors∪←ERR_MALFORMED
          :Else
              _Errors~←ERR_MALFORMED
          :EndIf
        ∇

        ∇ wchars←writeutf16 unicode;uni2wchars;illegal;add_illegal
    ⍝ Get the UTF-16 characters from Unicode scalars.
    ⍝ Don't forget to use the legalize function on the input before writing utf.
      ⍝ If Scalar > 0xFFFF
      ⍝     H ← (⌊(Scalar-0x10000)÷0x400) + 0xD800
      ⍝     L ← 0x400|(Scalar-0x10000) + 0xDC00
          uni2wchars←{
              ⍵<65536:⍵                                    ⍝  U+0000 →   U+FFFF : single character
              ⍵<1114112:55296 56320+1024 1024⊤⍵-65536      ⍝ U+10000 → U+10FFFF : split into surrogate pairs
              65533                                        ⍝ > U+10FFFF : relpace by U+FFFD
          }
          wchars←⊃,/uni2wchars¨unicode
          _Errors~←ERR_MALFORMED
        ∇


        ∇ unicode←legalize unicode;ill
    ⍝ Replace illegal Unicode characters by U+FFFD, and raise the illegal error flag.
    ⍝ boolean spotting illegal characters (cf http://www.unicode.org/faq/utf_bom.html#9)
          ill←unicode<0                    ⍝ Unicode scalars cannot be negative
          ill←ill∨32767=32768|⌊unicode÷2   ⍝ Unicode scalars ending in FFFE or FFFF
          ill←ill∨27=⌊unicode÷2048         ⍝ UTF-16 surrogates (range U+D800 - U+DFFF)
          ill←ill∨17≤⌊unicode÷65536        ⍝ Unicode scalars beyond U+10FFFF
          Illegal←ill/⍳⍴ill
          :If 0≠⍴Illegal
              _Errors∪←ERR_ILLEGAL
              unicode[Illegal]←65533        ⍝ replace illegal characters by U+FFFD
          :Else
              _Errors~←ERR_ILLEGAL
          :EndIf
        ∇


        ∇ unicode←toquadav newlines unicode;masks1;masks2;shiftr;i;written;mask;old;new;NL
    ⍝ Choose what newline sequence will be used (user defined or auto-detected...)
    ⍝ and replace any newline sequence by the chosen one, raising the ERR_NL flag if heterogeneous sequences are found.
    ⍝ When to ⎕AV : if the chosen sequence does not belong to ⎕AV, it will be symbolized by ⎕av[4]
    ⍝ When from ⎕AV : any lonely ⎕AV[4] character will be interpreted as the chosen sequence
          _NewLines←_NewLines∪⊂NL←(⌽⍲\2⍴V11)/13,4⊃_Unicode    ⍝ ⎕AV[4] is necessarily a newline sequence in V11 or else 13
          :If ~toquadav
              _Errors~←ERR_NL                       ⍝ when from ⎕AV : clear NL error before updating it
          :EndIf
         
          :If 0≠⍴_NewLineUser                       ⍝ user-defined newline sequence
              _NewLine←_NewLineUser
          :ElseIf toquadav∨(0=⍴_NewLine)            ⍝ do not auto-detect when from ⎕AV if _NewLine is already set
              masks1←_NewLines⍷¨⊂unicode            ⍝ masks of beginning of newline sequences (ambiguous : there can be several sequences on the same character, eg (13 10) and (,13)
              shiftr←{0∊⍺:⍵ ⋄ (⍴⍵)↑(-⍺+⍴⍵)↑⍵}       ⍝ shift vector to the right, filling with 0
              masks2←(⍴¨_NewLines){⊃(¯1+⍳⍺)∨.shiftr⊂⍵}¨masks1     ⍝ masks of newline characters of each sequence
              masks1←(↑masks1)∧<\[1]↑masks2         ⍝ unambiguous masks of beginning of sequences (priority given to the first ones in _NewLines)
              i←(((∨/[1]masks1)⍳1)⌷[2]masks1,0)⍳1   ⍝ search a match through rows then through columns
              _NewLine←i⊃_NewLines,_NewLines[1]     ⍝ pick the newline sequence, use the first one by default.
          :EndIf
         
          :If V11
          :AndIf toquadav∧(~_NewLine∧.∊⊂_Unicode)   ⍝ when toquadav : if the newline sequence is untranslatable
              new←,4⊃_Unicode                       ⍝ the newline sequence will be outputed as ⎕AV[4]
          :Else
              new←_NewLine
          :EndIf
         
          written←{0}¨unicode                       ⍝ mask of characters that already have been replaced, to prevent overwriting
         
          :For old :In (⊂_NewLine)∪_NewLines        ⍝ replace any newline sequence (old) by the chosen one (new)
              unicode←old,unicode                   ⍝ add a match at the beginning
              written←((⍴old)⍴0),written            ⍝ update written the same way
              mask←(old⍷unicode)>written            ⍝ mask of matching sequences (cannot overwrite)
              unicode written←(⊂mask)⊂¨unicode written  ⍝ partitioned at find points.
              :If toquadav>ERR_NL∊_Errors           ⍝ when getting...
              :AndIf (∨/1↓mask)∧(old≢_NewLine)      ⍝ non-default newline sequence detected
              :AndIf ∧/old∘≢¨,¨13 10                ⍝ CR & LF are accepted as homogeneous to the chosen sequence => no error
                  _Errors∪←ERR_NL                   ⍝ raise error
              :EndIf
              unicode←(⍴new)↓⊃,/{new,(⍴old)↓⍵}¨unicode          ⍝ replace NL by _NewLine, and remove the first match
              written←(⍴new)↓⊃,/{((⍴new)⍴1),(⍴old)↓⍵}¨written   ⍝ update written the same way
          :EndFor
        ∇


        ∇ string←unicode2apl codes;table;index
    ⍝ Translate from Unicode to ⎕AV
          :If V11
              index←_Unicode⍳codes
              string←(⎕AV,_Replacement)[index]
              Untranslatable←(index=1+⍴_Unicode)/⍳⍴codes
              :If 0≠⍴Untranslatable
                  _Errors∪←ERR_UNTRANS
              :Else
                  _Errors~←ERR_UNTRANS
              :EndIf
          :Else
              string←⎕UCS codes
          :EndIf
        ∇
        ∇ codes←apl2unicode string;index
    ⍝ Translate from ⎕AV to Unicode
          :If V11
              index←⎕AV⍳string
              codes←(_Unicode,65533)[index]
              Untranslatable←(index=1+⍴⎕AV)/⍳⍴string
              :If 0≠⍴Untranslatable
                  _Errors∪←ERR_UNTRANS
              :Else
                  _Errors~←ERR_UNTRANS
              :EndIf
          :Else
              codes←⎕UCS string
          :EndIf
        ∇


⍝==============================================================================⍝
⍝     Private Data                                                             ⍝
⍝==============================================================================⍝


    ⍝ There are currently 4 concurrent mapping from Unicode to ⎕AV :
    ⍝ A - U:\11.0\aplsrc\unicode.h
    ⍝ B - U:\11.0\comp\dyalognet.cs
    ⍝ C - )COPY U:\11.0\aplc\dyalogc.dws Compiler.UNICODE
    ⍝ D - (⎕NEW ⎕SE.UnicodeFile '').Unicode
    ⍝ This one is from A≡C, modified to use 180 96 at [219 237]
    ⍝ B uses the greek Beta (U+03b2) for ß≡⎕AV[140] instead of the german sharp S (U+00df)
    ⍝ D is completely different and as lots of zeros.
    num←{⍎4↓⍵} ⋄ x←⍬
        x,←num '  0:    0     8    10    13    32   12 57344 57345'
        x,←num '  8:   27     9  9014   619    37   39  9082  9077'
        x,←num ' 16:   95    97    98    99   100  101   102   103'
        x,←num ' 24:  104   105   106   107   108  109   110   111'
        x,←num ' 32:  112   113   114   115   116  117   118   119'
        x,←num ' 40:  120   121   122 57346 57347  175    46  9068'
        x,←num ' 48:   48    49    50    51    52   53    54    55'
        x,←num ' 56:   56    57 57348   164   165   36   163   162'
        x,←num ' 64: 8710    65    66    67    68   69    70    71'
        x,←num ' 72:   72    73    74    75    76   77    78    79'
        x,←num ' 80:   80    81    82    83    84   85    86    87'
        x,←num ' 88:   88    89    90 57349 57350  253   183 57351'
        x,←num ' 96: 9049   193   194   195   199  200   202   203'
        x,←num '104:  204   205   206   207   208  210   211   212'
        x,←num '112:  213   217   218   219   221  254   227   236'
        x,←num '120:  240   242   245   123  8866  125  8867  9015'
        x,←num '128:  168   192   196   197   198 9064   201   209'
        x,←num '136:  214   216   220   223   224  225   226   228'
        x,←num '144:  229   230   231   232   233  234   235   237'
        x,←num '152:  238   239   241    91    47 9023    92  9024'
        x,←num '160:   60  8804    61  8805    62 8800  8744  8743'
        x,←num '168:   45    43   247   215    63 8714  9076   126'
        x,←num '176: 8593  8595  9075  9675    42 8968  8970  8711'
        x,←num '184: 8728    40  8834  8835  8745 8746  8869  8868'
        x,←num '192:  124    59    44  9073  9074 9042  9035  9033'
        x,←num '200: 9021  8854  9055  9017    33 9045  9038  9067'
        x,←num '208: 9066  8801  8802   243   244  246   248    34'
        x,←num '216:   35 57352    38   180  9496 9488  9484  9492'
        x,←num '224: 9532  9472  9500  9508  9524 9516  9474    64'
        x,←num '232:  249   250   251    94   252   96  8739   182'
        x,←num '240:   58  9079   191   161  8900 8592  8594  9053'
        x,←num '248:   41    93 57353 57354   167 9109  9054  9059'
        :Field Private _Unicode←x

        x←''                                                 ⋄ ERR_OK←⍬
    ⍝ File Errors :
        x,←⊂'File does not exist'                            ⋄ ERR_EXIST←1       ⍝ updated in Import - cleared in Export
        x,←⊂'File Size not a multiple of Encoding Width'     ⋄ ERR_ODD←2         ⍝ updated in Import - cleared in Export
    ⍝ Encoding Errors :
        x,←⊂'Unmarked Encoding : Assuming UTF-8'             ⋄ ERR_NOBOM←3       ⍝ updated in Import - cleared in Export or writing Raw/Text/NestedText
        x,←⊂'Detected Encoding different from specified'     ⋄ ERR_BADBOM←4      ⍝ updated in Import - cleared in Export or writing Raw/Text/NestedText
        x,←⊂'Malformed UTF Characters'                       ⋄ ERR_MALFORMED←5   ⍝ updated in Import - cleared in Export or writing Raw/Text/NestedText
    ⍝ Unicode Errors :
        x,←⊂'Illegal Unicode Characters'                     ⋄ ERR_ILLEGAL←6     ⍝ updated in Import and when writing Raw/Text/NestedText
    ⍝ Text Errors :
        x,←⊂'Non homogeneous New Line sequences'             ⋄ ERR_NL←7          ⍝ updated when writing Text/NestedText - cleared when writing Raw
        x,←⊂'Untranslatable Characters to/from QuadAV'       ⋄ ERR_UNTRANS←8     ⍝ updated when writing Text/NestedText - cleared when writing Raw (not possible in unicode version)
        :Field Private _ErrorMessages←x


  ⍝ Format of _TableBOM :
    ⍝ 1st column : name : vector of uppercase strings used to retreive the name (See the Encoding property)
    ⍝ 2nd column : width of the encoding, in bytes
    ⍝ 3rd column : endianness : 0=N/A ; 1=Little ; 2=Big
    ⍝ 4th column : BOM (vector of bytes) (see http://en.wikipedia.org/wiki/Byte_Order_Mark)
    ⍝ the first rows pre-empt the following rows (the 1st match is used)
    ⍝ big endian should pre-empt little endian and be used by default according to the Unicode Standard (http://www.unicode.org/faq/utf_bom.html#36)
    ⍝ UTF-16 should pre-empt UTF-32, because this latter is very rarely used. User will have to specify UTF-32 to use it. This is not standard.
        x←0 4⍴''
        x,[1]←('UTF' '8') 1 0 (239 187 191)               ⍝ UTF-8                   ⍝ higher priority
        x,[1]←('UTF' '16' 'BE') 2 2 (254 255)             ⍝ UTF-16-BE
        x,[1]←('UTF' '16' 'LE') 2 1 (255 254)             ⍝ UTF-16-LE
        x,[1]←('UTF' '32' 'BE') 4 2 (0 0 254 255)         ⍝ UTF-32-BE
        x,[1]←('UTF' '32' 'LE') 4 1 (255 254 0 0)         ⍝ UTF-32-LE
        :Field Private _TableBOM←x
        ⎕ex'x' ⍝ get rid of this

        :Field Private _Encoding            ⍝ row of _TableBOM currently in use
        :Field Private _EncodingDefault←1   ⍝ default encoding - if you change it don't forget to change _ErrorMessages[5]
        :Field Private _EncodingUser←⍬      ⍝ encoding forced by the user (empty if none)

        :Property _EncodingName             ⍝ current name of encoding (see above)
            ∇ r←get
              r←⊃_TableBOM[_Encoding;1]
            ∇
        :EndProperty
        :Property _EncodingList             ⍝ list of handled encoding names
            ∇ r←get
              r←_TableBOM[;1]
            ∇
        :EndProperty
        :Property _Width                    ⍝ current width (see above)
            ∇ r←get
              r←⊃_TableBOM[_Encoding;2]
            ∇
        :EndProperty
        :Property _Endian                   ⍝ current endianness (see above)
            ∇ r←get
              r←⊃_TableBOM[_Encoding;3]
            ∇
        :EndProperty
        :Property _BOM                      ⍝ current BOM (see above)
            ∇ r←get
              r←⊃_TableBOM[_Encoding;4]
            ∇
        :EndProperty
        :Property _BOMlist                  ⍝ list of handled BOMs (see above)
            ∇ r←get
              r←_TableBOM[;4]
            ∇
        :EndProperty

⍝==============================================================================⍝
⍝     Debug stuff                                                              ⍝
⍝==============================================================================⍝

        ∇ r←getdebugdata;T;R;mal;ill;unt
          :Access Public
    ⍝ builds a matrix with one line of status below each line of text.
    ⍝ If the newline sequence is multi-char, indices will be messed up.
          T←Text
          R←(⍴T)⍴'·'
          mal ill←Malformed Illegal     ⍝ Text indices of Raw errors (can be moved because of newlines)
          unt←Untranslatable            ⍝ Text indices of Text errors (can not be moved)
          R[mal~mal∩unt]←'∆'
          R[mal∩unt]←'⍋'
          R[ill~ill∩unt]←'○'
          R[ill∩unt]←'⌽'
          R[unt~mal∪ill]←'|'
          T R←(⎕AV[4],T)('·',R)
          T R←(⊂T){1↓¨(⍺∊⎕AV[4 3])⊂⍵}¨T R
         
          r←⍬
          r,←⊂'· = OK ; ∆ = Malformed ; ○ = Illegal ; | = Untranslatable ; ⍋ = Malformed and Untranslatable ; ⌽ = Illegal and Untranslatable'
          r,←⊂''
          r,←,T,[1.5]R
        ∇

    :EndClass ⍝ UnicodeFile  $Revision: 21539 $

:EndNamespace
