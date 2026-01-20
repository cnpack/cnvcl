object FormComplex: TFormComplex
  Left = 192
  Top = 107
  Width = 974
  Height = 563
  Caption = 'Test Complex'
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
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 966
    Height = 531
    ActivePage = TabSheet1
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TCnComplexNumber'
      object grpComplex: TGroupBox
        Left = 16
        Top = 16
        Width = 929
        Height = 465
        Caption = 'Complex Number'
        TabOrder = 0
        object lbl1: TLabel
          Left = 184
          Top = 32
          Width = 11
          Height = 20
          Caption = '+'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl2: TLabel
          Left = 352
          Top = 32
          Width = 5
          Height = 20
          Caption = 'i'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl3: TLabel
          Left = 184
          Top = 120
          Width = 11
          Height = 20
          Caption = '+'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl4: TLabel
          Left = 352
          Top = 120
          Width = 5
          Height = 20
          Caption = 'i'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl5: TLabel
          Left = 176
          Top = 160
          Width = 11
          Height = 20
          Caption = '='
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtComplexAR: TEdit
          Left = 40
          Top = 32
          Width = 121
          Height = 21
          TabOrder = 0
          Text = '3'
        end
        object edtComplexAI: TEdit
          Left = 208
          Top = 32
          Width = 121
          Height = 21
          TabOrder = 1
          Text = '4'
        end
        object edtComplexBR: TEdit
          Left = 40
          Top = 120
          Width = 121
          Height = 21
          TabOrder = 2
          Text = '1'
        end
        object edtComplexBI: TEdit
          Left = 208
          Top = 120
          Width = 121
          Height = 21
          TabOrder = 3
          Text = '1'
        end
        object btnAdd: TButton
          Left = 40
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Add'
          TabOrder = 4
          OnClick = btnAddClick
        end
        object btnSub: TButton
          Left = 120
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Sub'
          TabOrder = 5
          OnClick = btnSubClick
        end
        object btnMul: TButton
          Left = 200
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Mul'
          TabOrder = 6
          OnClick = btnMulClick
        end
        object btnDiv: TButton
          Left = 280
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Div'
          TabOrder = 7
          OnClick = btnDivClick
        end
        object edtComplexResult: TEdit
          Left = 40
          Top = 192
          Width = 321
          Height = 21
          TabOrder = 8
        end
        object btnAbsolute: TButton
          Left = 392
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Absolute'
          TabOrder = 9
          OnClick = btnAbsoluteClick
        end
        object btnArgument: TButton
          Left = 480
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Argument'
          TabOrder = 10
          OnClick = btnArgumentClick
        end
        object btnSqrt: TButton
          Left = 392
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Sqrt'
          TabOrder = 11
          OnClick = btnSqrtClick
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TCnBigComplexDecimal'
      ImageIndex = 1
      object grpBigComplexDecimal: TGroupBox
        Left = 16
        Top = 16
        Width = 929
        Height = 465
        Caption = 'Big Complex Decimal'
        TabOrder = 0
        object lbl6: TLabel
          Left = 184
          Top = 32
          Width = 11
          Height = 20
          Caption = '+'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl7: TLabel
          Left = 352
          Top = 32
          Width = 5
          Height = 20
          Caption = 'i'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl8: TLabel
          Left = 184
          Top = 120
          Width = 11
          Height = 20
          Caption = '+'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object lbl9: TLabel
          Left = 352
          Top = 120
          Width = 5
          Height = 20
          Caption = 'i'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'MS Sans Serif'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object edtBigDecAR: TEdit
          Left = 40
          Top = 32
          Width = 121
          Height = 21
          TabOrder = 0
          Text = '1.5'
        end
        object edtBigDecAI: TEdit
          Left = 208
          Top = 32
          Width = 121
          Height = 21
          TabOrder = 1
          Text = '2.5'
        end
        object edtBigDecBR: TEdit
          Left = 40
          Top = 120
          Width = 121
          Height = 21
          TabOrder = 2
          Text = '3.0'
        end
        object edtBigDecBI: TEdit
          Left = 208
          Top = 120
          Width = 121
          Height = 21
          TabOrder = 3
          Text = '1.0'
        end
        object btnBigDecAdd: TButton
          Left = 40
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Add'
          TabOrder = 4
          OnClick = btnBigDecAddClick
        end
        object btnBigDecSub: TButton
          Left = 120
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Sub'
          TabOrder = 5
          OnClick = btnBigDecSubClick
        end
        object btnBigDecMul: TButton
          Left = 200
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Mul'
          TabOrder = 6
          OnClick = btnBigDecMulClick
        end
        object btnBigDecDiv: TButton
          Left = 280
          Top = 72
          Width = 57
          Height = 25
          Caption = 'Div'
          TabOrder = 7
          OnClick = btnBigDecDivClick
        end
        object edtBigDecResult: TEdit
          Left = 40
          Top = 192
          Width = 321
          Height = 21
          TabOrder = 8
        end
        object btnBigDecAbsolute: TButton
          Left = 392
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Absolute'
          TabOrder = 9
          OnClick = btnBigDecAbsoluteClick
        end
        object btnBigDecArgument: TButton
          Left = 480
          Top = 32
          Width = 75
          Height = 25
          Caption = 'Argument'
          TabOrder = 10
          OnClick = btnBigDecArgumentClick
        end
        object btnBigDecConjugate: TButton
          Left = 392
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Conjugate'
          TabOrder = 11
          OnClick = btnBigDecConjugateClick
        end
        object btnBigDecNegate: TButton
          Left = 480
          Top = 72
          Width = 75
          Height = 25
          Caption = 'Negate'
          TabOrder = 12
          OnClick = btnBigDecNegateClick
        end
        object btnBigDecSetZero: TButton
          Left = 392
          Top = 112
          Width = 75
          Height = 25
          Caption = 'Set Zero'
          TabOrder = 13
          OnClick = btnBigDecSetZeroClick
        end
        object btnBigDecSetOne: TButton
          Left = 480
          Top = 112
          Width = 75
          Height = 25
          Caption = 'Set One'
          TabOrder = 14
          OnClick = btnBigDecSetOneClick
        end
        object btnBigDecSetI: TButton
          Left = 392
          Top = 152
          Width = 75
          Height = 25
          Caption = 'Set I'
          TabOrder = 15
          OnClick = btnBigDecSetIClick
        end
        object edtBigDecDetail: TEdit
          Left = 40
          Top = 232
          Width = 321
          Height = 21
          TabOrder = 16
        end
      end
    end
  end
end
