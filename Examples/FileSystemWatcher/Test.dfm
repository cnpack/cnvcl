object Form1: TForm1
  Left = 185
  Top = 75
  BorderStyle = bsDialog
  Caption = 'FileSystemWatcher Test'
  ClientHeight = 549
  ClientWidth = 770
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lbl1: TLabel
    Left = 8
    Top = 16
    Width = 134
    Height = 13
    Caption = 'Select a Directory to Watch:'
  end
  object Memo1: TMemo
    Left = 240
    Top = 16
    Width = 519
    Height = 521
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 501
    Width = 113
    Height = 17
    Caption = 'Watch!'
    TabOrder = 1
    OnClick = CheckBox1Click
  end
  object DriveComboBox1: TDriveComboBox
    Left = 8
    Top = 40
    Width = 219
    Height = 19
    DirList = DirectoryListBox1
    TabOrder = 2
  end
  object DirectoryListBox1: TDirectoryListBox
    Left = 8
    Top = 72
    Width = 219
    Height = 97
    ItemHeight = 16
    TabOrder = 3
    OnChange = DirectoryListBox1Change
  end
  object CheckBox2: TCheckBox
    Left = 8
    Top = 477
    Width = 145
    Height = 17
    Caption = 'Include Watch Path'
    TabOrder = 4
    OnClick = CheckBox2Click
  end
  object GroupBox1: TGroupBox
    Left = 5
    Top = 325
    Width = 228
    Height = 137
    Caption = 'File Masks:'
    TabOrder = 5
    object Memo2: TMemo
      Left = 28
      Top = 24
      Width = 172
      Height = 73
      Lines.Strings = (
        '*.*')
      ScrollBars = ssVertical
      TabOrder = 0
    end
    object Button1: TButton
      Left = 76
      Top = 104
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 176
    Width = 228
    Height = 145
    Caption = 'NotifyFilters'
    TabOrder = 6
    object CheckBox3: TCheckBox
      Left = 5
      Top = 16
      Width = 105
      Height = 17
      Caption = 'FileNameChange'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object CheckBox4: TCheckBox
      Left = 125
      Top = 16
      Width = 97
      Height = 17
      Caption = 'DirNameChange'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object CheckBox5: TCheckBox
      Left = 5
      Top = 40
      Width = 97
      Height = 17
      Caption = 'AttributeChange'
      TabOrder = 2
    end
    object CheckBox6: TCheckBox
      Left = 125
      Top = 40
      Width = 97
      Height = 17
      Caption = 'SizeChange'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object CheckBox7: TCheckBox
      Left = 5
      Top = 64
      Width = 97
      Height = 17
      Caption = 'WriteChange'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object CheckBox8: TCheckBox
      Left = 125
      Top = 64
      Width = 97
      Height = 17
      Caption = 'AccessChange'
      TabOrder = 5
    end
    object CheckBox9: TCheckBox
      Left = 5
      Top = 88
      Width = 121
      Height = 17
      Caption = 'CreationDateChange'
      TabOrder = 6
    end
    object CheckBox10: TCheckBox
      Left = 125
      Top = 88
      Width = 97
      Height = 17
      Caption = 'SecurityChange'
      TabOrder = 7
    end
    object Button2: TButton
      Left = 68
      Top = 112
      Width = 75
      Height = 25
      Caption = 'OK'
      TabOrder = 8
      OnClick = Button2Click
    end
  end
  object FileSystemWatcher1: TCnFileSystemWatcher
    Active = False
    IncludePath = False
    FileMasks.Strings = (
      '*.*')
    WatchedDir = 'C:\'
    WatchSubTree = True
    NotifyFilters = [nfFileNameChange, nfDirNameChange, nfSizeChange, nfWriteChange]
    OnChange = CnFileSystemWatcherChange
    Left = 192
    Top = 472
  end
end
