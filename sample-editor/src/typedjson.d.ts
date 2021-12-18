declare module "typedjson" {

    export class TypedJsonFactory {
        static create(): TypedJson
        static withMetaSchema(): TypedJson
    }


    export class TypedJson {
        withSchema(typedJson: TypedJson): TypedJson
        forValue(json: string): TypedJson
        markers(): Marker[]
        suggestAt(offset: number): string[]
    }

    export interface Marker {
        start: number,
        end: number,
        pointer: string,
        message: string,
        severity: string
    }

}