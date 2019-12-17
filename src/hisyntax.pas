unit hisyntax;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs, SynEditHighlighterFoldBase;

type

  { THiSyntax }

  THiSyntax = class(TInterfacedObject, IHiSyntax)
  private
    fHighlighters: TStringList;
  public
    constructor Create();
    destructor Destroy;
    function GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
    function GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
  end;


implementation
uses
  Graphics, SynHighlighterPas,SynHighlighterXML,SynEditStrConst;

{ THiSyntax }

constructor THiSyntax.Create();
var
  SynHL: TSynCustomFoldHighlighter;
begin
  fHighlighters:=TStringList.Create;
  SynHL:=TSynPasSyn.Create(nil);
  SynHL.CommentAttribute.Foreground := clTeal;
  SynHL.IdentifierAttribute.Foreground := clNavy;
  SynHL.KeywordAttribute.Style := [fsBold];
  SynHL.StringAttribute.Foreground := clFuchsia;
  SynHL.WhitespaceAttribute.Background := clCream;

  fHighlighters.AddObject(SynHL.LanguageName, SynHL);
  SynHL:=TSynXmlSyn.Create(nil);
  fHighlighters.AddObject(SynHL.LanguageName, SynHL);
end;

destructor THiSyntax.Destroy;
var
  i: integer;
begin
  for i:=0 to fHighlighters.Count-1 do
    fHighlighters.Objects[i].Free;
  fHighlighters.Free;
end;

function THiSyntax.GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
var
  Index: integer;
begin
  Index := fHighlighters.IndexOf(ALang);
  if Index>=0 then
    Result := fHighlighters.Objects[Index] as TSynCustomFoldHighlighter
  else
    Result := nil;
end;

function THiSyntax.GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
var
  ext,Filter: string;
  synHL: TSynCustomFoldHighlighter;
  i,j,ExtLen: integer;
begin
  ext := ExtractFileExt(APath);
  ExtLen := Length(ext);
  for i:=0 to fHighlighters.Count-1 do
  begin
    synHL := fHighlighters.Objects[i] as TSynCustomFoldHighlighter;
    Filter := LowerCase(synHL.DefaultFilter);
    j := Pos('|', Filter);
    if j > 0 then
    begin
      Delete(Filter, 1, j);
      j := Pos(ext, Filter);
      if (j > 0) and ((j + ExtLen > Length(Filter)) or
        (Filter[j + ExtLen] = ';')) then
      begin
        Result := synHL;
        exit;
      end;
    end;
  end;
end;

end.

