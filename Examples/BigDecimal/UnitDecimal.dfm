object FormBigDecimal: TFormBigDecimal
  Left = 192
  Top = 108
  Width = 975
  Height = 563
  Caption = 'Test Big Decimal'
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
  object pgc1: TPageControl
    Left = 8
    Top = 8
    Width = 945
    Height = 513
    ActivePage = tsBigDecimal
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsBigDecimal: TTabSheet
      Caption = 'Big Decimal'
      object grpBigDecimal: TGroupBox
        Left = 8
        Top = 8
        Width = 921
        Height = 465
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Big Decimal'
        TabOrder = 0
        object lblDecimal: TLabel
          Left = 16
          Top = 24
          Width = 41
          Height = 13
          Caption = 'Decimal:'
        end
        object edtBigDecimal1: TEdit
          Left = 72
          Top = 20
          Width = 545
          Height = 21
          TabOrder = 0
          Text = '193289.02302002003020000000000000000000000000023828387188738238'
        end
        object btnSetAndGet: TButton
          Left = 640
          Top = 20
          Width = 153
          Height = 21
          Caption = 'Set Dec && Get String'
          TabOrder = 1
          OnClick = btnSetAndGetClick
        end
        object edtBigDecimal2: TEdit
          Left = 72
          Top = 52
          Width = 545
          Height = 21
          TabOrder = 2
          Text = '193289.02302002003020000000000000000000000000023828387188738238'
        end
        object btnRandCmp: TButton
          Left = 640
          Top = 51
          Width = 153
          Height = 21
          Caption = 'Compare'
          TabOrder = 3
          OnClick = btnRandCmpClick
        end
        object btnBigDecimalAdd: TButton
          Left = 72
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Add'
          TabOrder = 4
          OnClick = btnBigDecimalAddClick
        end
        object btnBigDecimalSub: TButton
          Left = 208
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Subtract'
          TabOrder = 5
          OnClick = btnBigDecimalSubClick
        end
        object btnBigDecimalMul: TButton
          Left = 344
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Multiply'
          TabOrder = 6
          OnClick = btnBigDecimalMulClick
        end
        object btnBigDecimalDivide: TButton
          Left = 488
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Divide'
          TabOrder = 7
          OnClick = btnBigDecimalDivideClick
        end
        object edtBigDecimalResult: TEdit
          Left = 8
          Top = 128
          Width = 897
          Height = 21
          TabOrder = 8
        end
        object btnSetFloat: TButton
          Left = 816
          Top = 20
          Width = 89
          Height = 21
          Caption = 'Set Float'
          TabOrder = 9
          OnClick = btnSetFloatClick
        end
        object edtFloat: TEdit
          Left = 816
          Top = 48
          Width = 89
          Height = 21
          TabOrder = 10
          Text = '-932.232401'
        end
      end
    end
  end
end
