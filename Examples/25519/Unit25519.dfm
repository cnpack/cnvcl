object Form25519: TForm25519
  Left = 192
  Top = 107
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
          OnClick = btnCurv25519MontLadderMulClick
        end
      end
    end
  end
end
