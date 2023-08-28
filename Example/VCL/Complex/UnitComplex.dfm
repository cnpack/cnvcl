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
  PixelsPerInch = 96
  TextHeight = 13
  object grpComplex: TGroupBox
    Left = 16
    Top = 16
    Width = 929
    Height = 505
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
