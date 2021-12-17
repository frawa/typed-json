declare module "typedjson" {

    export class SuggestFactory {
        static withMetaSchema(): Suggest
    }


    export class Suggest {
        forValue(json: string): Suggest
        at(offset: number): [string]
    }

}