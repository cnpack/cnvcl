object Form1: TForm1
  Left = 122
  Top = 208
  Width = 288
  Height = 379
  Caption = 'Validate Image Test'
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
    Color = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object Label1: TLabel
    Left = 25
    Top = 310
    Width = 88
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
    Top = 244
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
    Width = 92
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
    Top = 241
    Width = 121
    Height = 21
    TabOrder = 8
  end
  object Button1: TButton
    Left = 121
    Top = 272
    Width = 75
    Height = 25
    Caption = 'Validate'
    TabOrder = 9
    OnClick = Button1Click
  end
  object Edit2: TEdit
    Left = 121
    Top = 307
    Width = 121
    Height = 21
    TabOrder = 10
  end
  object CnSpinEdit1: TCnSpinEdit
    Left = 176
    Top = 84
    Width = 66
    Height = 22
    MaxValue = 10
    MinValue = 0
    TabOrder = 0
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
    TabOrder = 1
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
    TabOrder = 3
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
    TabOrder = 2
    Value = 4
    OnChange = CnSpinEdit4Change
  end
  object chkFixStyle: TCheckBox
    Left = 152
    Top = 192
    Width = 97
    Height = 17
    Caption = 'Fixed Style'
    TabOrder = 5
    OnClick = chkFixStyleClick
  end
  object chkFixColor: TCheckBox
    Left = 152
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Fixed Color'
    TabOrder = 7
    OnClick = chkFixColorClick
  end
  object chkFixPos: TCheckBox
    Left = 24
    Top = 216
    Width = 97
    Height = 17
    Caption = 'Fixed Position'
    TabOrder = 6
    OnClick = chkFixPosClick
  end
end
