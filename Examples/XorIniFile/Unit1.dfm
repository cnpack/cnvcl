object Form1: TForm1
  Left = 283
  Top = 216
  BorderStyle = bsDialog
  Caption = 'XOR Ini Test'
  ClientHeight = 293
  ClientWidth = 528
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 16
    Top = 16
    Width = 233
    Height = 257
    Caption = 'Write Xor INI'
    TabOrder = 0
    object lblName: TLabel
      Left = 16
      Top = 32
      Width = 37
      Height = 13
      Caption = 'Name ='
    end
    object lblPasswd: TLabel
      Left = 16
      Top = 64
      Width = 55
      Height = 13
      Caption = 'Password ='
    end
    object Bevel1: TBevel
      Left = 16
      Top = 96
      Width = 201
      Height = 18
      Shape = bsBottomLine
    end
    object lblXor: TLabel
      Left = 16
      Top = 136
      Width = 57
      Height = 13
      Caption = 'XOR Seed: '
    end
    object edtName: TEdit
      Left = 96
      Top = 32
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'CnPack'
    end
    object edtPasswd: TEdit
      Left = 96
      Top = 64
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '123456'
    end
    object edtSeed: TEdit
      Left = 96
      Top = 136
      Width = 121
      Height = 21
      TabOrder = 2
      Text = '1234'
    end
    object btnWrite: TButton
      Left = 16
      Top = 176
      Width = 201
      Height = 25
      Caption = 'Write Xor Ini File'
      TabOrder = 3
      OnClick = btnWriteClick
    end
    object btnView: TButton
      Left = 16
      Top = 216
      Width = 201
      Height = 25
      Caption = 'View this File'
      TabOrder = 4
      OnClick = btnViewClick
    end
  end
  object grp1: TGroupBox
    Left = 264
    Top = 16
    Width = 249
    Height = 257
    Caption = 'Read Xor INI'
    TabOrder = 1
    object btnRead: TButton
      Left = 16
      Top = 24
      Width = 217
      Height = 25
      Caption = 'Read this File'
      TabOrder = 0
      OnClick = btnReadClick
    end
    object mmo1: TMemo
      Left = 16
      Top = 64
      Width = 217
      Height = 177
      TabOrder = 1
    end
  end
end
