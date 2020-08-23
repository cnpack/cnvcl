object FormPrime: TFormPrime
  Left = 237
  Top = 137
  BorderStyle = bsDialog
  Caption = 'Prime Number Test'
  ClientHeight = 464
  ClientWidth = 687
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pgc1: TPageControl
    Left = 16
    Top = 16
    Width = 657
    Height = 429
    ActivePage = tsCRT
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsGenPrime: TTabSheet
      Caption = 'Find Prime Numbers'
      object btnGen: TButton
        Left = 16
        Top = 16
        Width = 209
        Height = 25
        Caption = 'Find Prime Numbers from 2 to'
        TabOrder = 0
        OnClick = btnGenClick
      end
      object edtMax: TEdit
        Left = 248
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 1
        Text = '65537'
      end
      object mmoResult: TMemo
        Left = 16
        Top = 56
        Width = 617
        Height = 325
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 2
      end
      object chkQuickGen: TCheckBox
        Left = 392
        Top = 16
        Width = 97
        Height = 17
        Caption = 'Quick Mode'
        Checked = True
        State = cbChecked
        TabOrder = 3
      end
    end
    object tsIsPrime: TTabSheet
      Caption = 'Is Prime'
      ImageIndex = 1
      object lblCheck: TLabel
        Left = 16
        Top = 28
        Width = 31
        Height = 13
        Caption = 'Check'
      end
      object lblInt64: TLabel
        Left = 16
        Top = 68
        Width = 31
        Height = 13
        Caption = 'Check'
      end
      object edtToPrime: TEdit
        Left = 64
        Top = 24
        Width = 121
        Height = 21
        TabOrder = 0
        Text = '39779'
      end
      object btnIsPrime: TButton
        Left = 208
        Top = 24
        Width = 75
        Height = 25
        Caption = 'Is Prime?'
        TabOrder = 1
        OnClick = btnIsPrimeClick
      end
      object edtInt64: TEdit
        Left = 64
        Top = 64
        Width = 121
        Height = 21
        TabOrder = 2
        Text = '397796406237767'
      end
      object btnInt64IsPrime: TButton
        Left = 208
        Top = 64
        Width = 105
        Height = 25
        Caption = 'Is Int64 Prime?'
        TabOrder = 3
        OnClick = btnInt64IsPrimeClick
      end
      object btnCarmichael: TButton
        Left = 336
        Top = 64
        Width = 297
        Height = 25
        Caption = 'Carmichael is Prime?'
        TabOrder = 4
        OnClick = btnCarmichaelClick
      end
      object mmoCar: TMemo
        Left = 16
        Top = 112
        Width = 617
        Height = 269
        Anchors = [akLeft, akTop, akRight, akBottom]
        ReadOnly = True
        ScrollBars = ssBoth
        TabOrder = 5
      end
      object btnGen64: TButton
        Left = 488
        Top = 24
        Width = 145
        Height = 25
        Caption = 'Generate a Int64 Prime'
        TabOrder = 6
        OnClick = btnGen64Click
      end
      object btnGenInt32Prime: TButton
        Left = 336
        Top = 24
        Width = 137
        Height = 25
        Caption = 'Generate a Int32 Prime'
        TabOrder = 7
        OnClick = btnGenInt32PrimeClick
      end
    end
    object tsMontgomery: TTabSheet
      Caption = 'Montgomery Power Mod'
      ImageIndex = 2
      object lbl1: TLabel
        Left = 208
        Top = 24
        Width = 6
        Height = 13
        Caption = '^'
      end
      object lblMonMod: TLabel
        Left = 416
        Top = 28
        Width = 20
        Height = 13
        Caption = 'mod'
      end
      object btn2: TSpeedButton
        Left = 168
        Top = 48
        Width = 23
        Height = 22
        Caption = 'J'
        Flat = True
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Wingdings 3'
        Font.Style = []
        ParentFont = False
        OnClick = btn2Click
      end
      object btn65537: TSpeedButton
        Left = 224
        Top = 48
        Width = 63
        Height = 22
        Caption = '65537'
        Flat = True
        OnClick = btn65537Click
      end
      object bvl1: TBevel
        Left = 24
        Top = 120
        Width = 609
        Height = 9
        Shape = bsTopLine
      end
      object lblMul: TLabel
        Left = 210
        Top = 148
        Width = 4
        Height = 13
        Caption = '*'
      end
      object lblMulMod: TLabel
        Left = 416
        Top = 148
        Width = 20
        Height = 13
        Caption = 'mod'
      end
      object bvl2: TBevel
        Left = 24
        Top = 232
        Width = 609
        Height = 9
        Shape = bsTopLine
      end
      object lblAddMod: TLabel
        Left = 210
        Top = 260
        Width = 6
        Height = 13
        Caption = '+'
      end
      object lbl3: TLabel
        Left = 416
        Top = 260
        Width = 20
        Height = 13
        Caption = 'mod'
      end
      object edtMonA: TEdit
        Left = 24
        Top = 24
        Width = 177
        Height = 21
        TabOrder = 0
        Text = '12345678987654321'
      end
      object edtMonB: TEdit
        Left = 224
        Top = 24
        Width = 185
        Height = 21
        TabOrder = 1
        Text = '6405181853089073441'
      end
      object edtMonC: TEdit
        Left = 448
        Top = 24
        Width = 185
        Height = 21
        TabOrder = 2
        Text = '9676503616299152957'
      end
      object btnMon: TButton
        Left = 24
        Top = 72
        Width = 81
        Height = 21
        Caption = 'Power Mod'
        TabOrder = 3
        OnClick = btnMonClick
      end
      object edtMonRes: TEdit
        Left = 128
        Top = 72
        Width = 401
        Height = 21
        TabOrder = 4
      end
      object btnMonPowerMod64: TButton
        Left = 552
        Top = 72
        Width = 83
        Height = 21
        Caption = 'Power Mod U64'
        TabOrder = 5
        OnClick = btnMonPowerMod64Click
      end
      object edtMulModA: TEdit
        Left = 24
        Top = 144
        Width = 177
        Height = 21
        TabOrder = 6
        Text = '12345678987654321'
      end
      object edtMulModB: TEdit
        Left = 224
        Top = 144
        Width = 185
        Height = 21
        TabOrder = 7
        Text = '12345678987654321'
      end
      object edtMulModC: TEdit
        Left = 448
        Top = 144
        Width = 185
        Height = 21
        TabOrder = 8
        Text = '9676503616299152957'
      end
      object edtMulModRes: TEdit
        Left = 128
        Top = 184
        Width = 401
        Height = 21
        TabOrder = 9
      end
      object btnMulMod: TButton
        Left = 24
        Top = 184
        Width = 81
        Height = 21
        Caption = 'Mul Mod'
        TabOrder = 10
        OnClick = btnMulModClick
      end
      object btnMulMod64: TButton
        Left = 552
        Top = 184
        Width = 83
        Height = 21
        Caption = 'Mul Mod U64'
        TabOrder = 11
        OnClick = btnMulMod64Click
      end
      object edtAddModA: TEdit
        Left = 24
        Top = 256
        Width = 177
        Height = 21
        TabOrder = 12
        Text = '9413234242316512726'
      end
      object edtAddModB: TEdit
        Left = 224
        Top = 256
        Width = 185
        Height = 21
        TabOrder = 13
        Text = '9413234242316512726'
      end
      object edtAddModC: TEdit
        Left = 448
        Top = 256
        Width = 185
        Height = 21
        TabOrder = 14
        Text = '9676503616299152957'
      end
      object btnAddMod: TButton
        Left = 24
        Top = 296
        Width = 81
        Height = 21
        Caption = 'Add Mod'
        TabOrder = 15
        OnClick = btnAddModClick
      end
      object btnAddMod64: TButton
        Left = 552
        Top = 296
        Width = 83
        Height = 21
        Caption = 'Add Mod U64'
        TabOrder = 16
        OnClick = btnAddMod64Click
      end
      object edtAddModRes: TEdit
        Left = 128
        Top = 296
        Width = 401
        Height = 21
        TabOrder = 17
      end
    end
    object tsDH: TTabSheet
      Caption = 'Diffie-Hellman (Int64)'
      ImageIndex = 3
      object lblInt64DHP: TLabel
        Left = 16
        Top = 20
        Width = 29
        Height = 13
        Caption = 'Prime:'
      end
      object lblInt64DHRoot: TLabel
        Left = 312
        Top = 20
        Width = 26
        Height = 13
        Caption = 'Root:'
      end
      object lblDHA: TLabel
        Left = 16
        Top = 88
        Width = 10
        Height = 13
        Caption = 'A:'
      end
      object lblXA: TLabel
        Left = 48
        Top = 88
        Width = 13
        Height = 13
        Caption = 'Xa'
      end
      object lblXb: TLabel
        Left = 48
        Top = 128
        Width = 13
        Height = 13
        Caption = 'Xb'
      end
      object lblB: TLabel
        Left = 16
        Top = 128
        Width = 10
        Height = 13
        Caption = 'B:'
      end
      object edtDHPrime: TEdit
        Left = 56
        Top = 16
        Width = 209
        Height = 21
        TabOrder = 0
      end
      object edtDHRoot: TEdit
        Left = 352
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 1
      end
      object btnGenInt64DH: TButton
        Left = 496
        Top = 16
        Width = 137
        Height = 21
        Caption = 'Generate Int64 DH'
        TabOrder = 2
        OnClick = btnGenInt64DHClick
      end
      object btnGenInt32DH: TButton
        Left = 496
        Top = 48
        Width = 137
        Height = 21
        Caption = 'Generate UInt32 DH'
        TabOrder = 3
        OnClick = btnGenInt32DHClick
      end
      object edtDHXa: TEdit
        Left = 72
        Top = 84
        Width = 121
        Height = 21
        TabOrder = 4
        Text = '123456'
      end
      object edtDHXb: TEdit
        Left = 72
        Top = 124
        Width = 121
        Height = 21
        TabOrder = 5
        Text = '654321'
      end
      object btnCalcXA: TButton
        Left = 208
        Top = 84
        Width = 49
        Height = 21
        Caption = 'Calc Ya'
        TabOrder = 6
        OnClick = btnCalcXAClick
      end
      object btnCalcYb: TButton
        Left = 208
        Top = 124
        Width = 49
        Height = 21
        Caption = 'Calc Yb'
        TabOrder = 7
        OnClick = btnCalcYbClick
      end
      object edtDHYa: TEdit
        Left = 272
        Top = 84
        Width = 129
        Height = 21
        TabOrder = 8
      end
      object edtDHYb: TEdit
        Left = 272
        Top = 124
        Width = 129
        Height = 21
        TabOrder = 9
      end
      object btnDHACKey: TButton
        Left = 416
        Top = 84
        Width = 65
        Height = 21
        Caption = 'A Calc Key'
        TabOrder = 10
        OnClick = btnDHACKeyClick
      end
      object btnDHBCK: TButton
        Left = 416
        Top = 124
        Width = 65
        Height = 21
        Caption = 'B Calc Key'
        TabOrder = 11
        OnClick = btnDHBCKClick
      end
      object edtAKey: TEdit
        Left = 496
        Top = 84
        Width = 137
        Height = 21
        TabOrder = 12
      end
      object edtBKey: TEdit
        Left = 496
        Top = 124
        Width = 137
        Height = 21
        TabOrder = 13
      end
      object btnDHIsRoot32: TButton
        Left = 56
        Top = 48
        Width = 137
        Height = 21
        Caption = 'Is Primitive Root  32'
        TabOrder = 14
        OnClick = btnDHIsRoot32Click
      end
      object btnDHIsPrimitiveRoot64: TButton
        Left = 216
        Top = 48
        Width = 137
        Height = 21
        Caption = 'Is Primitive Root  64'
        TabOrder = 15
        OnClick = btnDHIsPrimitiveRoot64Click
      end
      object btnDHRand: TButton
        Left = 24
        Top = 104
        Width = 41
        Height = 21
        Caption = 'Rand'
        TabOrder = 16
        OnClick = btnDHRandClick
      end
    end
    object tsCRT: TTabSheet
      Caption = 'Chinese Remainer Theorem'
      ImageIndex = 4
      object btnCRTTest: TButton
        Left = 24
        Top = 24
        Width = 75
        Height = 25
        Caption = 'CRT Test'
        TabOrder = 0
        OnClick = btnCRTTestClick
      end
    end
  end
end
