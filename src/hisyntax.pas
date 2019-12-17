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
  protected
    procedure LoadStandard;
    procedure LoadFacil;
  public
    constructor Create();
    destructor Destroy;
    function GetHighlighterByLanguageName(ALang: string): TSynCustomFoldHighlighter;
    function GetHighlighterByFileName(APath: string): TSynCustomFoldHighlighter;
  end;


implementation
uses
  Graphics, SynHighlighterPas,SynHighlighterXML,SynEditStrConst, SynFacilHighlighter,dialogs;

{ THiSyntax }

procedure THiSyntax.LoadStandard;
var
  SynHL: TSynCustomFoldHighlighter;
begin
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

procedure THiSyntax.LoadFacil;
var
  Folder: string;
  result: LongInt;
  sr: TSearchRec;
  hlt : TSynFacilSyn;
begin
  Folder:='facildata';
  result :=  FindFirst(folder+'/*.xml', faAnyFile, sr);
  while result = 0 do
  begin
    hlt := TSynFacilSyn.Create(nil);
    hlt.LoadFromFile(Folder+'/'+sr.Name);
    hlt.DefaultFilter:='*';
    hlt.ClearSpecials;
    fHighlighters.AddObject(ExtractFileName(sr.Name), hlt);
    result := FindNext(sr);
  end;
  FindClose(sr);
end;

constructor THiSyntax.Create();
begin
  fHighlighters:=TStringList.Create;
//  LoadStandard;
  LoadFacil;
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

