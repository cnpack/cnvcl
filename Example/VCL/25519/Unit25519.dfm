object Form25519: TForm25519
  Left = 192
  Top = 111
  Width = 979
  Height = 563
  Caption = '25519 Curves'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pgc25519: TPageControl
    Left = 8
    Top = 8
    Width = 953
    Height = 521
    ActivePage = ts25519
    TabOrder = 0
    object ts25519: TTabSheet
      Caption = '25519 Basic'
      object grp25519: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 473
        Caption = '25519 Basic'
        TabOrder = 0
        object bvl1: TBevel
          Left = 16
          Top = 152
          Width = 897
          Height = 25
          Shape = bsTopLine
        end
        object bvl11: TBevel
          Left = 16
          Top = 256
          Width = 897
          Height = 25
          Shape = bsTopLine
        end
        object btnCurve25519G: TButton
          Left = 16
          Top = 24
          Width = 185
          Height = 25
          Caption = 'Curve25519 G on Curve?'
          TabOrder = 0
          OnClick = btnCurve25519GClick
        end
        object btnEd25519G: TButton
          Left = 16
          Top = 64
          Width = 185
          Height = 25
          Caption = 'Ed25519 G on Curve?'
          TabOrder = 1
          OnClick = btnEd25519GClick
        end
        object btnCurve25519GAdd: TButton
          Left = 224
          Top = 24
          Width = 185
          Height = 25
          Caption = 'Curve25519 G Add'
          TabOrder = 2
          OnClick = btnCurve25519GAddClick
        end
        object btnEd25519GAdd: TButton
          Left = 224
          Top = 64
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Add'
          TabOrder = 3
          OnClick = btnEd25519GAddClick
        end
        object btnCurve25519GSub: TButton
          Left = 432
          Top = 24
          Width = 185
          Height = 25
          Caption = 'Curve25519 G Sub'
          TabOrder = 4
          OnClick = btnCurve25519GSubClick
        end
        object btnEd25519GSub: TButton
          Left = 432
          Top = 64
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Sub'
          TabOrder = 5
          OnClick = btnEd25519GSubClick
        end
        object btnCurve25519GMul: TButton
          Left = 640
          Top = 24
          Width = 185
          Height = 25
          Caption = 'Curve25519 G Mul'
          TabOrder = 6
          OnClick = btnCurve25519GMulClick
        end
        object btnEd25519GMul: TButton
          Left = 640
          Top = 64
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Mul'
          TabOrder = 7
          OnClick = btnEd25519GMulClick
        end
        object btnEd25519ExtendedAdd: TButton
          Left = 224
          Top = 104
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Extended Add'
          TabOrder = 8
          OnClick = btnEd25519ExtendedAddClick
        end
        object btnEd25519ExtendedMul: TButton
          Left = 640
          Top = 104
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Extended Mul'
          TabOrder = 9
          OnClick = btnEd25519ExtendedMulClick
        end
        object btnEd25519GenKey: TButton
          Left = 16
          Top = 176
          Width = 185
          Height = 25
          Caption = 'Ed25519 Gen Key'
          TabOrder = 10
          OnClick = btnEd25519GenKeyClick
        end
        object btnEd25519SignSample: TButton
          Left = 224
          Top = 176
          Width = 185
          Height = 25
          Caption = 'Ed25519 Sign Sample'
          TabOrder = 11
          OnClick = btnEd25519SignSampleClick
        end
        object btnEd25519PointData: TButton
          Left = 432
          Top = 176
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Point Data'
          TabOrder = 12
          OnClick = btnEd25519PointDataClick
        end
        object btnCurve25519DHKeyExchange: TButton
          Left = 640
          Top = 176
          Width = 185
          Height = 25
          Caption = 'Curve 25519 DH Key Exchange'
          TabOrder = 13
          OnClick = btnCurve25519DHKeyExchangeClick
        end
        object btnCalcSqrt: TButton
          Left = 16
          Top = 104
          Width = 185
          Height = 25
          Caption = 'Mont/Ed 25519 Check'
          TabOrder = 14
          OnClick = btnCalcSqrtClick
        end
        object btn25519PointConvert: TButton
          Left = 16
          Top = 216
          Width = 185
          Height = 25
          Caption = 'Curve/Ed 25519 PointConvert'
          TabOrder = 15
          OnClick = btn25519PointConvertClick
        end
        object btnCurv25519MontLadderDouble: TButton
          Left = 224
          Top = 216
          Width = 185
          Height = 25
          Caption = 'Curv25519 Mont Ladder Double'
          TabOrder = 16
          OnClick = btnCurv25519MontLadderDoubleClick
        end
        object btnCurv25519MontLadderAdd: TButton
          Left = 432
          Top = 216
          Width = 185
          Height = 25
          Caption = 'Curv25519 Mont Ladder Add'
          TabOrder = 17
          OnClick = btnCurv25519MontLadderAddClick
        end
        object btnCurv25519MontLadderMul: TButton
          Left = 640
          Top = 216
          Width = 185
          Height = 25
          Caption = 'Curv25519 Mont Ladder Mul'
          TabOrder = 18
          OnClick = btnCurv25519MontLadderMulClick
        end
        object btnBigNumberToField: TButton
          Left = 16
          Top = 272
          Width = 185
          Height = 25
          Caption = 'BigNumber To Field64'
          TabOrder = 19
          OnClick = btnBigNumberToFieldClick
        end
        object btnField64Mul: TButton
          Left = 224
          Top = 272
          Width = 185
          Height = 25
          Caption = 'Field64 Mul'
          TabOrder = 20
          OnClick = btnField64MulClick
        end
        object btnField64MulTime: TButton
          Left = 432
          Top = 272
          Width = 185
          Height = 25
          Caption = 'Field64 Mul Time'
          TabOrder = 21
          OnClick = btnField64MulTimeClick
        end
        object btnCurv25519MontLadderField64Double: TButton
          Left = 16
          Top = 312
          Width = 185
          Height = 25
          Caption = 'Curv25519 Mont Ladder Field Double'
          TabOrder = 22
          OnClick = btnCurv25519MontLadderField64DoubleClick
        end
        object btnCurv25519MontLadderField64Add: TButton
          Left = 224
          Top = 312
          Width = 185
          Height = 25
          Caption = 'Curv25519 Mont Ladder Field Add'
          TabOrder = 23
          OnClick = btnCurv25519MontLadderField64AddClick
        end
        object btnCurv25519MontLadderField64Mul: TButton
          Left = 432
          Top = 312
          Width = 185
          Height = 25
          Caption = 'Curv25519 Mont Ladder Field Mul'
          TabOrder = 24
          OnClick = btnCurv25519MontLadderField64MulClick
        end
        object btnField64Sub: TButton
          Left = 640
          Top = 272
          Width = 185
          Height = 25
          Caption = 'Field64 Sub'
          TabOrder = 25
          OnClick = btnField64SubClick
        end
        object btnField64Reduce: TButton
          Left = 640
          Top = 312
          Width = 185
          Height = 25
          Caption = 'Field64 Reduce'
          TabOrder = 26
          OnClick = btnField64ReduceClick
        end
        object btnEd25519ExtendedField64Add: TButton
          Left = 16
          Top = 360
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Extended Field Add'
          TabOrder = 27
          OnClick = btnEd25519ExtendedField64AddClick
        end
        object btnEd25519ExtendedField64Mul: TButton
          Left = 224
          Top = 360
          Width = 185
          Height = 25
          Caption = 'Ed25519 G Extended Field Mul'
          TabOrder = 28
          OnClick = btnEd25519ExtendedField64MulClick
        end
        object btn25519Field64Power2k: TButton
          Left = 432
          Top = 360
          Width = 185
          Height = 25
          Caption = 'Field64 Power && Power 2k'
          TabOrder = 29
          OnClick = btn25519Field64Power2kClick
        end
        object btn25519Field64PowerPMinus2: TButton
          Left = 640
          Top = 360
          Width = 185
          Height = 25
          Caption = 'Field64 Power p-2'
          TabOrder = 30
          OnClick = btn25519Field64PowerPMinus2Click
        end
        object btnCurve25519Test: TButton
          Left = 432
          Top = 104
          Width = 185
          Height = 25
          Caption = 'Curve 25519 Scalar'
          TabOrder = 31
          OnClick = btnCurve25519TestClick
        end
      end
    end
    object ts25519Sign: TTabSheet
      Caption = 'Ed25519 Sign/Verify'
      ImageIndex = 1
      object grpEd25519Sign: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 473
        Caption = 'Ed25519 Sign/Verify'
        TabOrder = 0
        object lblEd25519Priv: TLabel
          Left = 16
          Top = 24
          Width = 57
          Height = 13
          Caption = 'Private Key:'
        end
        object lblEd25519Pub: TLabel
          Left = 16
          Top = 56
          Width = 53
          Height = 13
          Caption = 'Public Key:'
        end
        object lblEd25519Msg: TLabel
          Left = 16
          Top = 92
          Width = 46
          Height = 13
          Caption = 'Message:'
        end
        object lblEd25519Sig: TLabel
          Left = 16
          Top = 124
          Width = 48
          Height = 13
          Caption = 'Signature:'
        end
        object edtEd25519Priv: TEdit
          Left = 88
          Top = 22
          Width = 473
          Height = 21
          TabOrder = 0
        end
        object edtEd25519Pub: TEdit
          Left = 88
          Top = 50
          Width = 473
          Height = 21
          TabOrder = 1
        end
        object btnEd25519Gen: TButton
          Left = 576
          Top = 24
          Width = 75
          Height = 49
          Caption = 'Gen Keys'
          TabOrder = 2
          OnClick = btnEd25519GenClick
        end
        object edtEd25519Message: TEdit
          Left = 88
          Top = 88
          Width = 473
          Height = 21
          TabOrder = 3
          Text = 'My Message to be Sign.'
        end
        object btnEd25519Sign: TButton
          Left = 576
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Sign'
          TabOrder = 4
          OnClick = btnEd25519SignClick
        end
        object edtEd25519Sig: TEdit
          Left = 88
          Top = 120
          Width = 817
          Height = 21
          TabOrder = 5
        end
        object btnEd25519Verify: TButton
          Left = 664
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Verify'
          TabOrder = 6
          OnClick = btnEd25519VerifyClick
        end
        object btnSignTime: TButton
          Left = 752
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Sign Time'
          TabOrder = 7
          OnClick = btnSignTimeClick
        end
        object btnVerifyTime: TButton
          Left = 840
          Top = 88
          Width = 75
          Height = 21
          Caption = 'Verify Time'
          TabOrder = 8
          OnClick = btnVerifyTimeClick
        end
        object btnEd25519SignFile: TButton
          Left = 752
          Top = 24
          Width = 75
          Height = 49
          Caption = 'Sign File'
          TabOrder = 9
          OnClick = btnEd25519SignFileClick
        end
        object btnEd25519VerifyFile: TButton
          Left = 840
          Top = 24
          Width = 75
          Height = 49
          Caption = 'Verify File'
          TabOrder = 10
          OnClick = btnEd25519VerifyFileClick
        end
        object btnEd25519LoadKeys: TButton
          Left = 664
          Top = 24
          Width = 75
          Height = 21
          Caption = 'Load Keys'
          TabOrder = 11
          OnClick = btnEd25519LoadKeysClick
        end
        object btnEd25519SaveKeys: TButton
          Left = 664
          Top = 52
          Width = 75
          Height = 21
          Caption = 'Save Keys'
          TabOrder = 12
          OnClick = btnEd25519SaveKeysClick
        end
      end
    end
    object ts448Basic: TTabSheet
      Caption = '448 Basic'
      ImageIndex = 2
      object grp448Basic: TGroupBox
        Left = 8
        Top = 8
        Width = 929
        Height = 473
        Caption = 'grp448Basic'
        TabOrder = 0
        object btn448CheckMap: TButton
          Left = 16
          Top = 24
          Width = 185
          Height = 25
          Caption = 'Check 448 (X,Y) <=> (U, V)'
          TabOrder = 0
          OnClick = btn448CheckMapClick
        end
        object btnCurve448Test: TButton
          Left = 400
          Top = 24
          Width = 185
          Height = 25
          Caption = 'Curve 448 Scalar'
          TabOrder = 1
          OnClick = btnCurve448TestClick
        end
        object btnConvert448Point: TButton
          Left = 224
          Top = 24
          Width = 161
          Height = 25
          Caption = 'Convert 448 Point'
          TabOrder = 2
          OnClick = btnConvert448PointClick
        end
        object btnCurve448GOn: TButton
          Left = 608
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Curve 448 G On Curve?'
          TabOrder = 3
          OnClick = btnCurve448GOnClick
        end
        object btnEd448GOn: TButton
          Left = 768
          Top = 24
          Width = 137
          Height = 25
          Caption = 'Ed 448 G On Curve?'
          TabOrder = 4
          OnClick = btnEd448GOnClick
        end
        object btnEd448PlainToPoint: TButton
          Left = 768
          Top = 64
          Width = 137
          Height = 25
          Caption = 'Ed 448 G Plain To Point'
          TabOrder = 5
          OnClick = btnEd448PlainToPointClick
        end
        object btnAnother448GOn: TButton
          Left = 608
          Top = 64
          Width = 137
          Height = 25
          Caption = 'Another 448 G On Curve?'
          TabOrder = 6
          OnClick = btnAnother448GOnClick
        end
        object btnConvertAnother448Point: TButton
          Left = 224
          Top = 64
          Width = 161
          Height = 25
          Caption = 'Convert Another 448 Point'
          TabOrder = 7
          OnClick = btnConvertAnother448PointClick
        end
        object btnCurve448DHKeyExchange: TButton
          Left = 400
          Top = 64
          Width = 185
          Height = 25
          Caption = 'Curve 448 DH Key Exchange'
          TabOrder = 8
          OnClick = btnCurve448DHKeyExchangeClick
        end
        object btnEd448CalcKey: TButton
          Left = 16
          Top = 64
          Width = 185
          Height = 25
          Caption = 'Ed448 Calc Key'
          TabOrder = 9
          OnClick = btnEd448CalcKeyClick
        end
        object btnEd448GAdd: TButton
          Left = 16
          Top = 104
          Width = 185
          Height = 25
          Caption = 'Ed448 G Add'
          TabOrder = 10
          OnClick = btnEd448GAddClick
        end
        object btnEd448GMul: TButton
          Left = 224
          Top = 104
          Width = 161
          Height = 25
          Caption = 'Ed448 G Mul'
          TabOrder = 11
          OnClick = btnEd448GMulClick
        end
      end
    end
  end
  object dlgOpen1: TOpenDialog
    Left = 788
    Top = 184
  end
  object dlgSave1: TSaveDialog
    Left = 828
    Top = 184
  end
end
