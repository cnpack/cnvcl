object Form1: TForm1
  Left = 242
  Top = 120
  Width = 448
  Height = 390
  Caption = '文件打包&解包'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object btnAddDirectory: TButton
    Left = 120
    Top = 24
    Width = 75
    Height = 25
    Caption = '增加文件夹'
    TabOrder = 2
    OnClick = btnAddDirectoryClick
  end
  object Addfile: TButton
    Left = 201
    Top = 24
    Width = 75
    Height = 25
    Caption = '增加文件'
    TabOrder = 3
    OnClick = AddfileClick
  end
  object btnUnpacking: TButton
    Left = 8
    Top = 64
    Width = 75
    Height = 25
    Caption = '解压所有到...'
    TabOrder = 4
    OnClick = btnUnpackingClick
  end
  object Button5: TButton
    Left = 8
    Top = 24
    Width = 106
    Height = 25
    Caption = '创建|打开打包文件'
    TabOrder = 1
    OnClick = Button5Click
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 1
    Width = 97
    Height = 17
    Caption = '使用zip压缩？'
    TabOrder = 0
    Visible = False
  end
  object tv1: TTreeView
    Left = 8
    Top = 110
    Width = 185
    Height = 231
    Indent = 19
    TabOrder = 6
    OnChange = tv1Change
  end
  object lv1: TListView
    Left = 216
    Top = 110
    Width = 201
    Height = 231
    Columns = <
      item
        Caption = 'FileName'
        Width = 180
      end>
    TabOrder = 7
    ViewStyle = vsReport
    OnChange = lv1Change
  end
  object Button1: TButton
    Left = 120
    Top = 64
    Width = 75
    Height = 25
    Caption = '解压选中到...'
    TabOrder = 5
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'cnpkg'
    Filter = 'all file|*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 320
    Top = 272
  end
  object SaveDialog1: TSaveDialog
    Filter = 'all file|*'
    Left = 288
    Top = 272
  end
end
