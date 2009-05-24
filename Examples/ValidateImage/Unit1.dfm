object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Validate Image Test'
  ClientHeight = 322
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object CnValidateImage1: TCnValidateImage
    Left = 25
    Top = 8
    Width = 193
    Height = 65
  end
  object Label1: TLabel
    Left = 25
    Top = 286
    Width = 77
    Height = 13
    Caption = 'Last Correct Value'
  end
  object Label2: TLabel
    Left = 25
    Top = 87
    Width = 85
    Height = 13
    Caption = 'FontRandomSeed'
  end
  object Label3: TLabel
    Left = 25
    Top = 228
    Width = 52
    Height = 13
    Caption = 'InputValue'
  end
  object Label4: TLabel
    Left = 25
    Top = 115
    Width = 55
    Height = 13
    Caption = 'NoiseCount'
  end
  object Label5: TLabel
    Left = 25
    Top = 167
    Width = 105
    Height = 13
    Caption = 'Limited Input Times'
  end
  object Label6: TLabel
    Left = 25
    Top = 141
    Width = 59
    Height = 13
    Caption = 'ValueLength'
  end
  object Edit1: TEdit
    Left = 121
    Top = 225
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 121
    Top = 252
    Width = 75
    Height = 25
    Caption = 'Validate'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 121
    Top = 283
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object CnSpinEdit1: TCnSpinEdit
    Left = 176
    Top = 84
    Width = 66
    Height = 22
    MaxValue = 10
    MinValue = 0
    TabOrder = 3
    Value = 6
    OnChange = CnSpinEdit1Change
  end
  object CheckBox1: TCheckBox
    Left = 25
    Top = 192
    Width = 97
    Height = 17
    Caption = 'CaseSensitive'
    TabOrder = 4
    OnClick = CheckBox1Click
  end
  object CnSpinEdit2: TCnSpinEdit
    Left = 176
    Top = 112
    Width = 66
    Height = 22
    Increment = 50
    MaxValue = 1000
    MinValue = 0
    TabOrder = 5
    Value = 100
    OnChange = CnSpinEdit2Change
  end
  object CnSpinEdit3: TCnSpinEdit
    Left = 177
    Top = 164
    Width = 66
    Height = 22
    MaxValue = 10
    MinValue = 0
    TabOrder = 6
    Value = 3
    OnChange = CnSpinEdit2Change
  end
  object CnSpinEdit4: TCnSpinEdit
    Left = 176
    Top = 138
    Width = 66
    Height = 22
    MaxValue = 10
    MinValue = 0
    TabOrder = 7
    Value = 4
    OnChange = CnSpinEdit4Change
  end
end
