object FormBigDecimal: TFormBigDecimal
  Left = 192
  Top = 109
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
        object btnRoundToScale: TButton
          Left = 16
          Top = 168
          Width = 75
          Height = 21
          Caption = 'Round To:'
          TabOrder = 11
          OnClick = btnRoundToScaleClick
        end
        object edtRoundDigits: TEdit
          Left = 104
          Top = 168
          Width = 65
          Height = 21
          TabOrder = 12
          Text = '4'
        end
        object mmoRound: TMemo
          Left = 16
          Top = 208
          Width = 225
          Height = 241
          Hint = 
            '往绝对值大的数取'#13#10'往绝对值小的数取，等于只留整数部分的 Trunc'#13#10'往' +
            '正无穷大取'#13#10'往负无穷大取'#13#10'四舍五入、入至绝对值大的数'#13#10'四舍六入五' +
            '成双、入至绝对值大的数'
          ParentShowHint = False
          ReadOnly = True
          ShowHint = True
          TabOrder = 13
        end
        object btnGetDigits: TButton
          Left = 184
          Top = 168
          Width = 129
          Height = 21
          Caption = 'Get Digits Counts'
          TabOrder = 14
          OnClick = btnGetDigitsClick
        end
        object chkMulDivPrecision: TCheckBox
          Left = 624
          Top = 92
          Width = 113
          Height = 17
          Caption = 'Mul/Div Precision'
          TabOrder = 15
        end
        object edtMulDivRoundDigits: TEdit
          Left = 744
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 16
          Text = '4'
        end
        object btnGetHighScale: TButton
          Left = 328
          Top = 168
          Width = 121
          Height = 21
          Caption = 'Get High Point Value'
          TabOrder = 17
          OnClick = btnGetHighScaleClick
        end
        object btnBigDecimalToFloat: TButton
          Left = 816
          Top = 88
          Width = 89
          Height = 21
          Caption = 'To Float'
          TabOrder = 18
          OnClick = btnBigDecimalToFloatClick
        end
        object btnBDSqrt: TButton
          Left = 464
          Top = 168
          Width = 75
          Height = 21
          Caption = 'Sqrt'
          TabOrder = 19
          OnClick = btnBDSqrtClick
        end
        object btnDecimalToRational: TButton
          Left = 656
          Top = 168
          Width = 121
          Height = 21
          Caption = 'Decimal To Rational'
          TabOrder = 20
          OnClick = btnDecimalToRationalClick
        end
        object btnRationalToDecimal: TButton
          Left = 784
          Top = 168
          Width = 121
          Height = 21
          Caption = 'Rational To Decimal'
          TabOrder = 21
          OnClick = btnRationalToDecimalClick
        end
        object btnSqrt2: TButton
          Left = 560
          Top = 168
          Width = 75
          Height = 21
          Caption = 'Sqrt2'
          TabOrder = 22
          OnClick = btnSqrt2Click
        end
      end
    end
    object tsBigBinary: TTabSheet
      Caption = 'Big Binary'
      ImageIndex = 1
      object grpBigBinary: TGroupBox
        Left = 8
        Top = 8
        Width = 921
        Height = 465
        Caption = 'grpBigBinary'
        TabOrder = 0
        object lblBigBinary: TLabel
          Left = 16
          Top = 24
          Width = 41
          Height = 13
          Caption = 'Decimal:'
        end
        object edtBigBinary1: TEdit
          Left = 72
          Top = 20
          Width = 545
          Height = 21
          TabOrder = 0
          Text = '193289.02'
        end
        object edtBigBinary2: TEdit
          Left = 72
          Top = 52
          Width = 545
          Height = 21
          TabOrder = 1
          Text = '193289.02302002003020000000000000000000000000023828387188738238'
        end
        object btnBigBinarySetGet: TButton
          Left = 640
          Top = 20
          Width = 153
          Height = 21
          Caption = 'Set Dec && Get String'
          TabOrder = 2
          OnClick = btnBigBinarySetGetClick
        end
        object btnBigBinarySetFloat: TButton
          Left = 816
          Top = 20
          Width = 89
          Height = 21
          Caption = 'Set Float'
          TabOrder = 3
          OnClick = btnBigBinarySetFloatClick
        end
        object btnBigBinaryCompare: TButton
          Left = 640
          Top = 51
          Width = 153
          Height = 21
          Caption = 'Compare'
          TabOrder = 4
          OnClick = btnBigBinaryCompareClick
        end
        object edtBigBinaryFloat: TEdit
          Left = 816
          Top = 48
          Width = 89
          Height = 21
          TabOrder = 5
          Text = '-932.232401'
        end
        object btnBigBinaryAdd: TButton
          Left = 72
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Add'
          TabOrder = 6
          OnClick = btnBigBinaryAddClick
        end
        object btnBigBinarySub: TButton
          Left = 208
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Subtract'
          TabOrder = 7
          OnClick = btnBigBinarySubClick
        end
        object btnBigBinaryMul: TButton
          Left = 344
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Multiply'
          TabOrder = 8
          OnClick = btnBigBinaryMulClick
        end
        object btnBigBinaryDiv: TButton
          Left = 488
          Top = 88
          Width = 121
          Height = 21
          Caption = 'Divide'
          TabOrder = 9
          OnClick = btnBigBinaryDivClick
        end
        object chkBigBinaryPrecision: TCheckBox
          Left = 624
          Top = 92
          Width = 113
          Height = 17
          Caption = 'Mul/Div Precision'
          TabOrder = 10
        end
        object edtBBMulDivRoundDigits: TEdit
          Left = 744
          Top = 88
          Width = 49
          Height = 21
          TabOrder = 11
          Text = '4'
        end
        object edtBigBinaryResult: TEdit
          Left = 8
          Top = 128
          Width = 897
          Height = 21
          TabOrder = 12
        end
        object btnBigBinaryToFloat: TButton
          Left = 816
          Top = 88
          Width = 89
          Height = 21
          Caption = 'To Float'
          TabOrder = 13
          OnClick = btnBigBinaryToFloatClick
        end
      end
    end
  end
end
