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
      end
    end
  end
end
