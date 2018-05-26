unit UnitBer;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, CnTree;

type
  TFormParseBer = class(TForm)
    mmoResult: TMemo;
    btnParse: TButton;
    tv1: TTreeView;
    lblBin: TLabel;
    edtFile: TEdit;
    btnBrowse: TButton;
    dlgOpen: TOpenDialog;
    procedure btnParseClick(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tv1DblClick(Sender: TObject);
  private
    FHints: TStrings;
    procedure SaveNode(ALeaf: TCnLeaf; ATreeNode: TTreeNode;
      var Valid: Boolean);
  public
    { Public declarations }
  end;

var
  FormParseBer: TFormParseBer;

implementation

uses
  CnBerParser;

{$R *.DFM}

const
  PRIVATE_ARRAY: array[0..1191] of Byte = (
    $30, $82, $04, $A4, $02, $01, $00, $02, $82, $01, $01, $00, $BE, $CA, $0C, $6A,
    $D5, $3B, $D9, $5C, $BA, $12, $EA, $75, $21, $47, $5A, $4E, $DE, $41, $39, $E2,
    $3B, $09, $8A, $6A, $58, $EF, $58, $B5, $A4, $E1, $65, $0D, $5D, $E6, $A5, $9F,
    $35, $EF, $77, $07, $80, $5E, $52, $8F, $DA, $A2, $F4, $4B, $B2, $27, $23, $59,
    $AD, $34, $D5, $16, $F1, $78, $A0, $42, $19, $12, $DD, $70, $37, $6E, $48, $B3,
    $30, $ED, $23, $FC, $C9, $C6, $2F, $D5, $34, $2A, $49, $5C, $55, $26, $11, $D3,
    $D2, $9A, $D2, $7B, $1C, $48, $6E, $AD, $DB, $B0, $EF, $EE, $2F, $EA, $43, $A2,
    $6F, $50, $68, $F7, $EE, $CB, $1B, $07, $9E, $F2, $67, $32, $7D, $70, $75, $88,
    $A1, $81, $BA, $25, $F9, $A5, $44, $D4, $81, $0F, $0D, $2E, $7D, $90, $73, $65,
    $E7, $F3, $EB, $26, $8D, $36, $F9, $55, $40, $8F, $2E, $F2, $05, $29, $3D, $CF,
    $DA, $EB, $70, $60, $1E, $86, $88, $5C, $81, $C9, $C9, $00, $70, $F6, $C9, $E1,
    $6F, $EC, $FD, $2F, $C8, $AE, $14, $DF, $2A, $05, $82, $30, $C1, $81, $DF, $7B,
    $E0, $40, $AE, $89, $F2, $1E, $CB, $FD, $F4, $2A, $FE, $67, $AD, $78, $DF, $E0,
    $2D, $44, $B9, $27, $62, $A4, $C6, $DF, $4A, $06, $B3, $56, $E7, $3B, $4F, $13,
    $DE, $6C, $20, $43, $3D, $46, $B7, $80, $30, $B9, $5E, $88, $14, $1C, $E6, $95,
    $1E, $AC, $F9, $88, $D6, $A4, $7A, $C4, $25, $3B, $0A, $01, $06, $EE, $01, $37,
    $B3, $03, $4C, $97, $1D, $5E, $ED, $4D, $5B, $9E, $A6, $AD, $02, $03, $01, $00,
    $01, $02, $82, $01, $01, $00, $B5, $62, $DC, $89, $59, $2E, $EC, $49, $27, $32,
    $C2, $AB, $17, $37, $82, $19, $47, $5E, $95, $CC, $8A, $BA, $58, $59, $21, $32,
    $74, $91, $45, $2F, $70, $E6, $53, $66, $EC, $01, $40, $C2, $E8, $D4, $3D, $EB,
    $EE, $EC, $8B, $3D, $AF, $AA, $A3, $B6, $67, $FA, $63, $1C, $BE, $96, $70, $7E,
    $71, $46, $46, $16, $54, $66, $D2, $9C, $4C, $0B, $E8, $B1, $1C, $2C, $C2, $1C,
    $64, $EF, $7B, $07, $D4, $F5, $2E, $92, $E2, $12, $13, $C1, $53, $E8, $ED, $8B,
    $C3, $86, $11, $3E, $D7, $ED, $08, $AA, $DD, $20, $7A, $59, $E5, $62, $0A, $2A,
    $19, $09, $96, $C8, $F8, $E9, $DC, $AE, $65, $5F, $BF, $C4, $D9, $18, $2F, $8F,
    $3B, $B0, $18, $FF, $19, $54, $7B, $FE, $4F, $AF, $A8, $4C, $60, $7F, $8B, $56,
    $C0, $79, $A9, $39, $3B, $26, $90, $C4, $FD, $2B, $CB, $25, $DD, $F9, $0E, $82,
    $A7, $FA, $E6, $7C, $3E, $64, $FC, $26, $62, $47, $89, $94, $CB, $C5, $31, $77,
    $C9, $E4, $EC, $3A, $84, $0A, $44, $09, $BF, $7A, $C1, $D9, $C9, $2B, $AD, $A3,
    $61, $95, $4A, $A5, $14, $F8, $E4, $1A, $FF, $C7, $2C, $53, $73, $75, $CD, $8A,
    $8D, $59, $82, $2A, $11, $A0, $36, $32, $7B, $71, $FA, $EA, $B9, $99, $19, $ED,
    $8B, $1E, $02, $05, $A6, $18, $43, $B0, $C0, $23, $2D, $DB, $1F, $E5, $79, $04,
    $EC, $0D, $8F, $9D, $19, $ED, $29, $20, $CC, $1A, $4F, $2A, $48, $13, $37, $5D,
    $B7, $7F, $B9, $BD, $8F, $F5, $02, $81, $81, $00, $E9, $4D, $09, $D7, $89, $86,
    $75, $19, $00, $0F, $CC, $8C, $BA, $F3, $B6, $1D, $C6, $30, $BD, $72, $06, $78,
    $A8, $C9, $50, $BC, $F3, $5A, $DA, $6D, $C8, $EE, $8A, $2C, $37, $2C, $D1, $87,
    $30, $2D, $6A, $6B, $95, $B9, $57, $5C, $39, $67, $FE, $E2, $D1, $B9, $E3, $FE,
    $BB, $BB, $0E, $1A, $C2, $5F, $19, $0C, $63, $3A, $0D, $55, $EF, $F0, $4F, $68,
    $3B, $3E, $30, $97, $F9, $44, $FD, $E4, $9F, $BD, $A1, $6B, $3A, $AB, $82, $FB,
    $B2, $4C, $4F, $2C, $86, $22, $48, $2D, $20, $36, $BA, $87, $47, $E2, $35, $29,
    $D6, $85, $9D, $43, $97, $E4, $FE, $14, $42, $1D, $44, $91, $C9, $43, $77, $A8,
    $9C, $90, $94, $C0, $D3, $F1, $E9, $BC, $87, $E7, $02, $81, $81, $00, $D1, $5A,
    $25, $A2, $B2, $DE, $83, $34, $85, $E0, $15, $44, $7F, $A9, $98, $9B, $44, $5B,
    $E9, $4F, $BF, $83, $0A, $94, $DF, $F9, $41, $DB, $4E, $32, $37, $25, $5C, $B5,
    $88, $EA, $34, $6E, $E8, $88, $8C, $7A, $7B, $F9, $11, $03, $4D, $F7, $57, $11,
    $71, $0B, $D5, $65, $39, $BF, $32, $3F, $C1, $A6, $66, $07, $54, $FF, $72, $9A,
    $BD, $25, $09, $68, $1E, $45, $9E, $D4, $85, $D4, $F8, $B1, $06, $E1, $95, $0C,
    $D4, $00, $87, $77, $08, $C1, $88, $0B, $41, $F0, $62, $FA, $DB, $66, $C7, $12,
    $9C, $6B, $39, $4B, $22, $E9, $5A, $21, $9C, $B4, $A5, $54, $A6, $EF, $DA, $DF,
    $71, $EB, $40, $D6, $40, $54, $2C, $78, $E9, $70, $9B, $93, $BA, $4B, $02, $81,
    $81, $00, $82, $BF, $CB, $29, $41, $88, $DA, $6D, $75, $6C, $65, $4D, $E5, $A4,
    $9A, $47, $1B, $80, $CB, $5B, $71, $27, $04, $29, $37, $17, $7E, $27, $47, $CD,
    $49, $53, $EE, $9A, $13, $1E, $0A, $1B, $82, $B3, $A1, $35, $C4, $46, $88, $20,
    $46, $87, $F3, $00, $39, $5B, $BD, $DC, $50, $67, $E7, $E2, $B5, $56, $48, $9A,
    $79, $AD, $DA, $35, $74, $70, $30, $80, $90, $DB, $2C, $AE, $AF, $63, $AC, $0B,
    $0A, $3F, $63, $8C, $CD, $E7, $BB, $53, $C6, $3C, $20, $0F, $2A, $E9, $61, $9B,
    $F0, $B6, $DB, $58, $DF, $BF, $75, $DE, $09, $BC, $FE, $43, $18, $88, $9C, $A0,
    $CB, $E0, $CD, $42, $0B, $12, $C5, $A7, $F8, $7E, $CA, $EF, $3A, $99, $14, $4E,
    $B5, $B7, $02, $81, $80, $18, $0D, $CD, $89, $36, $18, $0E, $E7, $38, $02, $99,
    $2E, $F7, $A4, $1A, $DE, $AC, $2A, $0B, $52, $98, $64, $8C, $10, $B2, $F3, $46,
    $B7, $D5, $F2, $3B, $17, $D5, $E1, $1D, $CC, $1B, $1E, $2B, $25, $95, $4E, $75,
    $74, $16, $9A, $54, $98, $F8, $AE, $0E, $59, $17, $B9, $BB, $BA, $D6, $21, $31,
    $34, $D7, $40, $3C, $69, $E5, $57, $16, $28, $6D, $3E, $43, $4C, $28, $85, $AE,
    $A2, $DB, $B8, $2B, $5C, $3B, $ED, $EF, $65, $E8, $2A, $AB, $1C, $E1, $6E, $B9,
    $B2, $CF, $0C, $97, $AC, $E5, $6E, $A3, $FE, $45, $E4, $F6, $2B, $ED, $A9, $33,
    $02, $26, $84, $E5, $71, $65, $AA, $8A, $C2, $46, $FE, $D7, $01, $54, $E2, $C7,
    $9F, $A2, $A0, $84, $A5, $02, $81, $80, $77, $51, $EB, $95, $B5, $65, $29, $AB,
    $55, $52, $EE, $41, $99, $97, $13, $22, $A5, $42, $ED, $B2, $BC, $DF, $58, $7C,
    $4E, $64, $0C, $E7, $22, $D2, $0E, $95, $72, $9D, $A2, $99, $04, $AC, $45, $76,
    $6C, $1A, $1F, $EF, $CB, $8C, $67, $F4, $6F, $DC, $6D, $05, $F1, $E6, $E3, $7D,
    $2E, $34, $90, $83, $4A, $D3, $81, $64, $D4, $89, $24, $FE, $AA, $7A, $C1, $52,
    $03, $03, $B4, $20, $31, $4B, $AF, $D0, $C1, $B3, $6F, $A7, $67, $F7, $29, $C4,
    $EF, $5B, $C8, $51, $33, $FB, $B9, $7C, $FC, $B5, $B6, $3D, $02, $97, $FD, $80,
    $16, $94, $E2, $19, $5C, $12, $7B, $E7, $30, $5C, $5D, $10, $80, $A3, $26, $B9,
    $19, $E6, $5F, $18, $4E, $D0, $ED, $58
  );

procedure TFormParseBer.btnParseClick(Sender: TObject);
var
  Parser: TCnBerParser;
  Mem: TMemoryStream;
begin
  Parser := nil;
  Mem := nil;

  try
    if not FileExists(edtFile.Text) then
    begin
      Parser := TCnBerParser.Create(@PRIVATE_ARRAY[0], SizeOf(PRIVATE_ARRAY));
    end
    else
    begin
      Mem := TMemoryStream.Create;
      Mem.LoadFromFile(edtFile.Text);
      Parser := TCnBerParser.Create(Mem.Memory, Mem.Size);
    end;

    Parser.OnSaveNode := SaveNode;
    FHints.Clear;
    Parser.DumpToTreeView(tv1);
    if tv1.Items.Count > 0 then
      tv1.Items[0].Expand(True);

    mmoResult.Clear;
    mmoResult.Lines.Add('TotalCount: ' + IntToStr(Parser.TotalCount));
  finally
    Parser.Free;
    Mem.Free;
  end;
end;

procedure TFormParseBer.btnBrowseClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edtFile.Text := dlgOpen.FileName;
end;

function HexDumpMemory(AMem: Pointer; Size: Integer): string;
var
  I, J, DestP, PrevLineStart, Remain: Integer;
  AChar: Char;

  function HexValueHigh(AChar: Char): Char;
  var
    AByte: Byte;
  begin
    AByte := Ord(AChar) shr 4;
    if AByte in [0..9] then
      Inc(AByte, Ord('0'))
    else
      Inc(AByte, Ord('A') - 10);
    Result := Chr(AByte);
  end;
  
  function HexValueLow(AChar: Char): Char;
  var
    AByte: Byte;
  begin
    AByte := Ord(AChar) and $F;
    if AByte in [0..9] then
      Inc(AByte, Ord('0'))
    else
      Inc(AByte, Ord('A') - 10);
    Result := Chr(AByte);
  end;

begin
  if (Size <= 0) or (AMem = nil) then
  begin
    Result := '';
    Exit;
  end;

  SetLength(Result, ((Size div 16) + 1 ) * ( 3 * 16 + 2 + 16 + 2 ));
  FillChar(Result[1], Length(Result), 0);

  DestP := 0; PrevLineStart := 0;
  for I := 0 to Size - 1 do
  begin
    AChar := (PChar(Integer(AMem) + I))^;
    Inc(DestP);
    Result[DestP] := HexValueHigh(AChar);
    Inc(DestP);
    Result[DestP] := HexValueLow(AChar);
    if I < Size then
    begin
      Inc(DestP);
      if (I > 0) and ((I + 1) mod 16 = 0) then
      begin
        // DONE: 加缩略字符再加回车
        Result[DestP] := ' '; // 加空格分隔
        Inc(DestP);
        Result[DestP] := ';'; // 加分号分隔
        Inc(DestP);
        Result[DestP] := ' '; // 加空格分隔
        Inc(DestP);

        for J := PrevLineStart to I do
        begin
          AChar := (PChar(Integer(AMem) + J))^;
          if AChar in [#32..#127] then
            Result[DestP] := AChar
          else
            Result[DestP] := '.'; // 不可显示字符
          Inc(DestP);
        end;
        PrevLineStart := I + 1;

        Result[DestP] := #$D;
        Inc(DestP);
        Result[DestP] := #$A; // 加回车分隔
      end
      else
      begin
        Result[DestP] := ' '; // 加空格分隔
      end;
    end;
  end;

  Remain := Size mod 16;
  if Remain > 0 then
  begin
    // DONE: 处理末行未完的情形
    Remain := 16 - Remain; // 补充空格以对齐

    for I := 1 to Remain do
    begin
      Result[DestP] := ' '; // 加空格分隔
      Inc(DestP);
      Result[DestP] := ' '; // 加高位空格充当十六进制低位
      Inc(DestP);
      Result[DestP] := ' '; // 加低位空格充当十六进制低位
      Inc(DestP);
    end;

    Result[DestP] := ' '; // 加空格分隔
    Inc(DestP);
    Result[DestP] := ';'; // 加分号分隔
    Inc(DestP);
    Result[DestP] := ' '; // 加空格分隔
    Inc(DestP);

    for J := PrevLineStart to Size - 1 do
    begin
      AChar := (PChar(Integer(AMem) + J))^;
      if AChar in [#32..#127] then
        Result[DestP] := AChar
      else
        Result[DestP] := '.'; // 不可显示字符
      Inc(DestP);
    end;
  end;
end;

procedure TFormParseBer.SaveNode(ALeaf: TCnLeaf; ATreeNode: TTreeNode;
  var Valid: Boolean);
var
  Mem: Pointer;
  BerNode: TCnBerNode;
begin
  if not (ALeaf is TCnBerNode) then
    Exit;

  BerNode := ALeaf as TCnBerNode;
  ATreeNode.Text := BerNode.Text;

  if BerNode.BerDataLength > 65536 then
  begin
    FHints.Add('Data Too Long');
    ATreeNode.Data := Pointer(FHints.Count - 1);
    Exit;
  end
  else
  begin
    Mem := GetMemory(BerNode.BerDataLength);
    if Mem <> nil then
    begin
      BerNode.CopyDataTo(Mem);
      FHints.Add(HexDumpMemory(Mem, BerNode.BerDataLength));
      ATreeNode.Data := Pointer(FHints.Count - 1);
      FreeMemory(Mem);
    end;
  end;
end;

procedure TFormParseBer.FormCreate(Sender: TObject);
begin
  FHints := TStringList.Create;
end;

procedure TFormParseBer.FormDestroy(Sender: TObject);
begin
  FHints.Free;
end;

procedure TFormParseBer.tv1DblClick(Sender: TObject);
begin
  if tv1.Selected <> nil then
    ShowMessage(FHints[Integer(tv1.Selected.Data)]);
end;

end.
