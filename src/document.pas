unit document;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, intfs;

type

  { TDocument }

  TDocument = class(TInterfacedObject, IDocument)
  public
    procedure OpenFile(AFileName: string);
    //constructor Create(ASheet: TEdSheet; ASynEdit: TSynEdit);
  end;


implementation

{ TDocument }

procedure TDocument.OpenFile(AFileName: string);
begin

end;

end.

