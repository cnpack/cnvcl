object FormZip: TFormZip
  Left = 192
  Top = 107
  Width = 701
  Height = 521
  Caption = 'Test Zip'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 673
    Height = 473
    ActivePage = tsReader
    TabOrder = 0
    object tsReader: TTabSheet
      Caption = 'Open a Zip and Extract'
      object lblZip: TLabel
        Left = 16
        Top = 28
        Width = 37
        Height = 13
        Caption = 'Zip File:'
      end
      object lblPassword: TLabel
        Left = 432
        Top = 28
        Width = 49
        Height = 13
        Caption = 'Password:'
      end
      object edtZip: TEdit
        Left = 64
        Top = 24
        Width = 209
        Height = 21
        TabOrder = 0
      end
      object btnBrowse: TButton
        Left = 280
        Top = 24
        Width = 65
        Height = 21
        Caption = 'Browse'
        TabOrder = 1
        OnClick = btnBrowseClick
      end
      object btnRead: TButton
        Left = 360
        Top = 24
        Width = 65
        Height = 21
        Caption = 'Read Zip'
        TabOrder = 2
        OnClick = btnReadClick
      end
      object mmoZip: TMemo
        Left = 16
        Top = 64
        Width = 633
        Height = 369
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ReadOnly = True
        TabOrder = 3
      end
      object btnExtract: TButton
        Left = 584
        Top = 24
        Width = 65
        Height = 21
        Caption = 'Extract'
        TabOrder = 4
        OnClick = btnExtractClick
      end
      object edtPassword: TEdit
        Left = 488
        Top = 24
        Width = 89
        Height = 21
        TabOrder = 5
        Text = '123'
      end
    end
    object tsWriter: TTabSheet
      Caption = 'Create a New Zip'
      ImageIndex = 1
      object lblPass: TLabel
        Left = 400
        Top = 24
        Width = 49
        Height = 13
        Caption = 'Password:'
      end
      object mmoFiles: TMemo
        Left = 16
        Top = 64
        Width = 633
        Height = 369
        TabOrder = 0
      end
      object btnCreate: TButton
        Left = 16
        Top = 22
        Width = 75
        Height = 21
        Caption = 'Create'
        TabOrder = 1
        OnClick = btnCreateClick
      end
      object btnAdd: TButton
        Left = 104
        Top = 22
        Width = 75
        Height = 21
        Caption = 'Add a File'
        TabOrder = 2
        OnClick = btnAddClick
      end
      object btnSave: TButton
        Left = 192
        Top = 22
        Width = 75
        Height = 21
        Caption = 'Save'
        TabOrder = 3
        OnClick = btnSaveClick
      end
      object btnZipDir: TButton
        Left = 576
        Top = 20
        Width = 75
        Height = 21
        Caption = 'Zip Directory'
        TabOrder = 4
        OnClick = btnZipDirClick
      end
      object chkRemovePath: TCheckBox
        Left = 280
        Top = 24
        Width = 97
        Height = 17
        Caption = 'Remove Path'
        Checked = True
        State = cbChecked
        TabOrder = 5
      end
      object edtPass: TEdit
        Left = 464
        Top = 20
        Width = 105
        Height = 21
        TabOrder = 6
        Text = '123'
      end
    end
  end
  object dlgOpen: TOpenDialog
    Filter = '*.zip|*.zip'
    Left = 408
    Top = 8
  end
  object dlgSave: TSaveDialog
    Filter = '*.zip|*.zip'
    Left = 116
    Top = 112
  end
  object dlgOpenFile: TOpenDialog
    Left = 164
    Top = 112
  end
end
