declare module "typedjson" {

    export class TypedJsonFactory {
        static withMetaSchema(): TypedJson
    }


    export class TypedJson {
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