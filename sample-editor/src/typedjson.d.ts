declare module "typedjson" {
    export class TmpMain {
        static hello(): void
    }

    export class SuggestFactory {
        static withMetaSchema(): Suggest
    }


    export class Suggest {
        forValue(json: string): Suggest
        at(offset: number): [string]
    }

}