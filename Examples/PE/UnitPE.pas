unit UnitPE;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, ExtCtrls, StdCtrls, Psapi, CnPE, CnNative;

type
  TFormPE = class(TForm)
    lblPEFile: TLabel;
    edtPEFile: TEdit;
    btnParsePE: TButton;
    cbbRunModule: TComboBox;
    btnBrowse: TButton;
    btnParsePEFile: TButton;
    bvl1: TBevel;
    pgcPE: TPageControl;
    tsDosHeader: TTabSheet;
    tsNtHeader: TTabSheet;
    dlgOpen1: TOpenDialog;
    mmoDos: TMemo;
    mmoFile: TMemo;
    mmoOptional: TMemo;
    tsSectionHeader: TTabSheet;
    mmoSection: TMemo;
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnParsePEFileClick(Sender: TObject);
    procedure btnParsePEClick(Sender: TObject);
  private
    procedure DumpPE(PE: TCnPE);
  public
    { Public declarations }
  end;

var
  FormPE: TFormPE;

implementation

{$R *.DFM}

procedure TFormPE.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
    edtPEFile.Text := dlgOpen1.FileName;
end;

procedure TFormPE.FormCreate(Sender: TObject);
var
  HP: array of THandle;
  I, L: Integer;
  Cnt: DWORD;
  F: array[0..MAX_PATH] of Char;
begin
  // 获取 Module 列表
  if EnumProcessModules(GetCurrentProcess, nil, 0, Cnt) then
  begin
    if Cnt > 0 then
    begin
      L := Cnt div SizeOf(THandle);
      SetLength(HP, L);
      if EnumProcessModules(GetCurrentProcess, @HP[0], Cnt, Cnt) then
      begin
        for I := 0 to L - 1 do
        begin
          GetModuleFileName(HP[I], @F[0], MAX_PATH);
          cbbRunModule.Items.AddObject(IntToHex(HP[I], 8) + ' - ' + F, Pointer(HP[I]));
        end;
      end;
      cbbRunModule.ItemIndex := 0;
    end;
  end;
end;

procedure TFormPE.btnParsePEFileClick(Sender: TObject);
var
  PE: TCnPE;
begin
  mmoDos.Clear;
  mmoFile.Clear;
  mmoOptional.Clear;
  mmoSection.Clear;

  PE := TCnPE.Create(edtPEFile.Text);
  try
    PE.ParsePE;
    DumpPE(PE);
  finally
    PE.Free;
  end;
end;

procedure TFormPE.DumpPE(PE: TCnPE);
var
  I: Integer;

  function DumpByte(AB: Byte): string;
  begin
    Result := Format('%2.2x', [AB]);
  end;

  function DumpWord(AW: Word): string;
  begin
    Result := Format('%4.4x', [AW]);
  end;

  function DumpDWord(AD: DWORD): string;
  begin
    Result := Format('%8.8x', [AD]);
  end;

  function DumpTUInt64(AU: TUInt64): string;
  begin
    Result := Format('%16.16x', [AU]);
  end;

  function DumpBoolean(AB: Boolean): string;
  begin
    if AB then
      Result := 'True'
    else
      Result := 'False';
  end;

  function DumpPointer(AP: Pointer): string;
  begin
{$IFDEF CPUX64}
    Result := Format('%16.16x', [TCnNativeUInt(AP)]);
{$ELSE}
    Result := Format('%8.8x', [TCnNativeUInt(AP)]);
{$ENDIF}
  end;

  procedure D(M: TMemo; const N, V: string);
  begin
    M.Lines.Add(N + ': ' + V);
  end;

begin
  D(mmoDos, 'DosMagic', DumpWord(PE.DosMagic));
  D(mmoDos, 'DosCblp', DumpWord(PE.DosCblp));
  D(mmoDos, 'DosCp', DumpWord(PE.DosCp));
  D(mmoDos, 'DosCrlc', DumpWord(PE.DosCrlc));
  D(mmoDos, 'DosCparhdr', DumpWord(PE.DosCparhdr));
  D(mmoDos, 'DosMinalloc', DumpWord(PE.DosMinalloc));
  D(mmoDos, 'DosMaxalloc', DumpWord(PE.DosMaxalloc));
  D(mmoDos, 'DosSs', DumpWord(PE.DosSs));
  D(mmoDos, 'DosSp', DumpWord(PE.DosSp));
  D(mmoDos, 'DosCsum', DumpWord(PE.DosCsum));
  D(mmoDos, 'DosIp', DumpWord(PE.DosIp));
  D(mmoDos, 'DosCs', DumpWord(PE.DosCs));
  D(mmoDos, 'DosLfarlc', DumpWord(PE.DosLfarlc));
  D(mmoDos, 'DosOvno', DumpWord(PE.DosOvno));
  D(mmoDos, 'DosOemid', DumpWord(PE.DosOemid));
  D(mmoDos, 'DosOeminfo', DumpWord(PE.DosOeminfo));
  D(mmoDos, 'DosLfanew', DumpDWord(PE.DosLfanew));

  D(mmoFile, 'Signature', DumpDWORD(PE.Signature));

  D(mmoFile, 'FileMachine', DumpWord(PE.FileMachine));
  D(mmoFile, 'FileNumberOfSections', DumpWord(PE.FileNumberOfSections));
  D(mmoFile, 'FileTimeDateStamp', DumpDWORD(PE.FileTimeDateStamp));
  D(mmoFile, 'FilePointerToSymbolTable', DumpDWORD(PE.FilePointerToSymbolTable));
  D(mmoFile, 'FileNumberOfSymbols', DumpDWORD(PE.FileNumberOfSymbols));
  D(mmoFile, 'FileSizeOfOptionalHeader', DumpWord(PE.FileSizeOfOptionalHeader));
  D(mmoFile, 'FileCharacteristics', DumpWord(PE.FileCharacteristics));

  D(mmoOptional, 'OptionalMagic', DumpWord(PE.OptionalMagic));
  D(mmoOptional, 'OptionalMajorLinkerVersion', DumpByte(PE.OptionalMajorLinkerVersion));
  D(mmoOptional, 'OptionalMinorLinkerVersion', DumpByte(PE.OptionalMinorLinkerVersion));
  D(mmoOptional, 'OptionalSizeOfCode', DumpDWORD(PE.OptionalSizeOfCode));
  D(mmoOptional, 'OptionalSizeOfInitializedData', DumpDWORD(PE.OptionalSizeOfInitializedData));
  D(mmoOptional, 'OptionalSizeOfUninitializedData', DumpDWORD(PE.OptionalSizeOfUninitializedData));
  D(mmoOptional, 'OptionalAddressOfEntryPoint', DumpDWORD(PE.OptionalAddressOfEntryPoint));
  D(mmoOptional, 'OptionalBaseOfCode', DumpDWORD(PE.OptionalBaseOfCode));
  D(mmoOptional, 'OptionalBaseOfData', DumpDWORD(PE.OptionalBaseOfData));

  if PE.IsWin32 then
    D(mmoOptional, 'OptionalImageBase', DumpDWORD(PE.OptionalImageBase));
  if PE.IsWin64 then
    D(mmoOptional, 'OptionalImageBase64', DumpTUInt64(PE.OptionalImageBase64));

  D(mmoOptional, 'OptionalSectionAlignment', DumpDWORD(PE.OptionalSectionAlignment));
  D(mmoOptional, 'OptionalFileAlignment', DumpDWORD(PE.OptionalFileAlignment));
  D(mmoOptional, 'OptionalMajorOperatingSystemVersion', DumpWord(PE.OptionalMajorOperatingSystemVersion));
  D(mmoOptional, 'OptionalMinorOperatingSystemVersion', DumpWord(PE.OptionalMinorOperatingSystemVersion));
  D(mmoOptional, 'OptionalMajorImageVersion', DumpWord(PE.OptionalMajorImageVersion));
  D(mmoOptional, 'OptionalMinorImageVersion', DumpWord(PE.OptionalMinorImageVersion));
  D(mmoOptional, 'OptionalMajorSubsystemVersion', DumpWord(PE.OptionalMajorSubsystemVersion));
  D(mmoOptional, 'OptionalMinorSubsystemVersion', DumpWord(PE.OptionalMinorSubsystemVersion));
  D(mmoOptional, 'OptionalWin32VersionValue', DumpDWORD(PE.OptionalWin32VersionValue));
  D(mmoOptional, 'OptionalSizeOfImage', DumpDWORD(PE.OptionalSizeOfImage));
  D(mmoOptional, 'OptionalSizeOfHeaders', DumpDWORD(PE.OptionalSizeOfHeaders));
  D(mmoOptional, 'OptionalCheckSum', DumpDWORD(PE.OptionalCheckSum));
  D(mmoOptional, 'OptionalSubsystem', DumpWord(PE.OptionalSubsystem));
  D(mmoOptional, 'OptionalDllCharacteristics', DumpWord(PE.OptionalDllCharacteristics));

  if PE.IsWin32 then
    D(mmoOptional, 'OptionalSizeOfStackReserve', DumpDWORD(PE.OptionalSizeOfStackReserve));
  if PE.IsWin64 then
    D(mmoOptional, 'OptionalSizeOfStackReserve64', DumpTUInt64(PE.OptionalSizeOfStackReserve64));
  if PE.IsWin32 then
    D(mmoOptional, 'OptionalSizeOfStackCommit', DumpDWORD(PE.OptionalSizeOfStackCommit));
  if PE.IsWin64 then
    D(mmoOptional, 'OptionalSizeOfStackCommit64', DumpTUInt64(PE.OptionalSizeOfStackCommit64));
  if PE.IsWin32 then
    D(mmoOptional, 'OptionalSizeOfHeapReserve', DumpDWORD(PE.OptionalSizeOfHeapReserve));
  if PE.IsWin64 then
    D(mmoOptional, 'OptionalSizeOfHeapReserve64', DumpTUInt64(PE.OptionalSizeOfHeapReserve64));
  if PE.IsWin32 then
    D(mmoOptional, 'OptionalSizeOfHeapCommit', DumpDWORD(PE.OptionalSizeOfHeapCommit));
  if PE.IsWin64 then
    D(mmoOptional, 'OptionalSizeOfHeapCommit64', DumpTUInt64(PE.OptionalSizeOfHeapCommit64));
  D(mmoOptional, 'OptionalLoaderFlags', DumpDWORD(PE.OptionalLoaderFlags));
  D(mmoOptional, 'OptionalNumberOfRvaAndSizes', DumpDWORD(PE.OptionalNumberOfRvaAndSizes));

  // Dump 其他内容
  D(mmoFile, '----', '----');
  D(mmoFile, 'IsWin32', DumpBoolean(PE.IsWin32));
  D(mmoFile, 'IsWin64', DumpBoolean(PE.IsWin64));
  D(mmoFile, 'IsExe', DumpBoolean(PE.IsExe));
  D(mmoFile, 'IsDll', DumpBoolean(PE.IsDll));

  D(mmoOptional, '----', '----');
  for I := 0 to PE.OptionalNumberOfRvaAndSizes - 1 do
  begin
    D(mmoOptional, Format('DataDirectory %d VirtualAddress', [I]), DumpDWord(PE.DataDirectoryVirtualAddress[I]));
    D(mmoOptional, Format('DataDirectory %d Size', [I]), DumpDWord(PE.DataDirectorySize[I]));
  end;

  for I := 0 to PE.FileNumberOfSections - 1 do
  begin
    D(mmoSection, Format('Section %d Address', [I]), DumpPointer(PE.SectionHeader[I]));
  end;
end;

procedure TFormPE.btnParsePEClick(Sender: TObject);
var
  H: HMODULE;
  PE: TCnPE;
begin
  if cbbRunModule.ItemIndex < 0 then
    Exit;

  mmoDos.Clear;
  mmoFile.Clear;
  mmoOptional.Clear;
  mmoSection.Clear;

  H := HMODULE(cbbRunModule.Items.Objects[cbbRunModule.ItemIndex]);
  PE := TCnPE.Create(H);
  try
    PE.ParsePE;
    DumpPE(PE);
  finally
    PE.Free;
  end;
end;

end.
