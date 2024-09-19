const secondsPerDay = 24 * 60 * 60;

function debounce(func, wait, immediate, context) {
    var result;
    var timeout = null;
    return function () {
        var ctx = context || this, args = arguments;
        var later = function () {
            timeout = null;
            if (!immediate) result = func.apply(ctx, args);
        };
        var callNow = immediate && !timeout;
        // Tant que la fonction est appelée, on reset le timeout.
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
        if (callNow) {
            result = func.apply(ctx, args);
        }
        return result;
    };
}

const generateTimelinePdf = debounce((title, groups, config) => generatePDF(generate(groups, config), title), 250, false);
// const generateTimelinePdf = debounce((groups, config) => console.log(generate(groups, config)), 100, false);

async function addFilters() {
    let addselect = document.getElementById("addfilters");
    var selected = addselect.value;
    filters[selected] = { include: [... new Set(rawtable[selected])] };

    let select = document.getElementById("filters");

    let options = metas.filter(m => m.colId == selected).forEach(
        m => select.add(new Option(m.label, m.colId)));


    addselect.remove(addselect.selectedIndex);

    saveFilters();
}

async function deleteFilter() {
    delete filters[currentFilter];

    saveFilters();
}

async function editFilter() {
    let select = document.getElementById("filters");
    let selected = select.selectedOptions;

    if (selected.length == 0) {
        document.getElementById("filter-control").innerHTML = "";
        document.getElementById("filter").innerHTML = "";
        currentFilter = undefined;

    } else {
        let option = selected[0];
        currentFilter = option.value;
        let include = filters[option.value].include;
        let values = [... new Set(rawtable[option.value])];
        let meta = metas.find(m => m.colId == option.value);
        if (meta == -1) return;

        if (meta.type.startsWith("Ref:")) {
            let table = meta.type.substring(4);
            let refa = await grist.docApi.fetchTable(table);
            let refmetas = await getTypes(table);
            let visibleCol = refmetas.find(rm => rm.id == meta.visibleCol);
            if (visibleCol == undefined) return;

            kvs = values.map((v, i) => ({ key: i, value: v, selected: include.includes(v), label: refa[visibleCol.colId][refa.id.indexOf(v)] }));
        } else {

            kvs = values.map((v, i) => ({ key: i, value: v, selected: include.includes(v), label: v }));
        }

        document.getElementById("filter-control").innerHTML = '<button onClick="selectAll()">Tous</button> <button onClick="selectNone()">Aucun</button> <button onClick="deleteFilter()">Suppr. filtre</button><br />';
        document.getElementById("filter").innerHTML =  kvs.map(kv =>
            `<input type="checkbox" id="i` + kv.key + `" name="v` + kv.key + `" ` + (kv.selected ? "checked" : "") + ` onClick="clickFilter(` + kv.key + `)">
            <label for="v`+ kv.key + `">` + kv.label + `</label><br>`
        ).join("");


    }


}

function selectAll() {
    filters[currentFilter] = { include: [... new Set(rawtable[currentFilter])] };
    editFilter();
    saveFilters();
}

function selectNone() {
    filters[currentFilter] = { include: [] };
    editFilter();
    saveFilters();

}

function clickFilter(key) {
    let v = kvs.find(kv => kv.key == key).value;
    let i = filters[currentFilter].include.indexOf(v);

    if (i > -1)
        delete filters[currentFilter].include[i];

    if (document.getElementById("i" + key).checked) {
        filters[currentFilter].include.push(v);
    }

    saveFilters();

}

async function saveFilters() {
    const table = await grist.getTable();
    await table.update({
        id: currentId,
        fields: { [_mappings.filters]: JSON.stringify(filters) }
    });
}

let filters = {};
let metas = [];
let rawtable = [];
let kvs = [];
let currentFilter = undefined;
let currentId = undefined;
let _mappings;

window.addEventListener('load', (event) => {
    console.log('La page est complètement chargée');

    let mappedRecord = [];
    let mycols = [];
    let newSelection;
    let options = undefined;

    // const urlParams = new URLSearchParams(window.location.search);
    // const isReadOnly = urlParams.get('readonly') === 'true' ||
    //     (urlParams.has('access') && urlParams.get('access') !== 'full');
    // const docTimeZone = urlParams.get('timeZone');





    grist.ready({
        requiredAccess: 'full',
        columns: [
            {
                name: "title", // What field we will read.
                title: "Titre", // Friendly field name.
                optional: false, // Is this an optional field.
                type: "Text", // What type of column we expect.
                //   description: "D", // Description of a field.
                allowMultiple: false, // Allows multiple column assignment.
                strictType: true
            },
            {
                name: "fromDate",
                title: "Début",
                optional: true,
                type: "DateTime",
                allowMultiple: false,
                strictType: true
            },
            {
                name: "toDate",
                title: "Fin",
                optional: true,
                type: "DateTime",
                allowMultiple: false,
                strictType: true
            },
            {
                name: "table",
                title: "Table",
                optional: false,
                type: "Text",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "sort",
                title: "Tri",
                optional: true,
                type: "Text",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "fromField",
                title: "Champ Début",
                optional: true,
                type: "Text",
                allowMultiple: false,
                strictType: true
            },
            {
                name: "toField",
                title: "Champ Fin",
                optional: false,
                type: "Text",
                allowMultiple: false,
                strictType: true
            },

            {
                name: "groupe",
                title: "Grouper par",
                optional: false,
                type: "Any",
                allowMultiple: false
            },
            {
                name: "sousGroupe",
                title: "Puis grouper par",
                optional: true,
                type: "Any",
                allowMultiple: false
            },

            {
                name: "color",
                title: "Couleur",
                optional: true,
                type: "Any",
                allowMultiple: false

            },

            {
                name: "content",
                title: "Contenu",
                optional: true,
                type: "Any",
                // type: "Text, Int, Numeric",
                //   description: "D",
                allowMultiple: false,
                strictType: false
            },

            {
                name: "orientation",
                title: "Orientation",
                optional: false,
                type: "Any",
                allowMultiple: false,
                strictType: false
            },

            {
                name: "papersize",
                title: "Taille papier",
                optional: false,
                type: "Any",
                allowMultiple: false,
                strictType: false
            },

            {
                name: "vpages",
                title: "Pages verticales",
                optional: false,
                type: "Int",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "hpages",
                title: "Pages horizontales",
                optional: false,
                type: "Int",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "groupsFontSize",
                title: "Taille groupes",
                optional: false,
                type: "Numeric",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "tasksFontSize",
                title: "Taille tâches",
                optional: false,
                type: "Numeric",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "hoursFontSize",
                title: "Taille horaires",
                optional: false,
                type: "Numeric",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "mode",
                title: "Orientation timeline",
                optional: false,
                type: "Any",
                allowMultiple: false,
                strictType: false
            },

            {
                name: "align",
                title: "Alignement",
                optional: false,
                type: "Any",
                allowMultiple: false,
                strictType: false
            },
            {
                name: "multiline",
                title: "Multiligne",
                optional: false,
                type: "Bool",
                allowMultiple: false,
                strictType: true
            },

            {
                name: "filters",
                title: "Filtres",
                optional: false,
                type: "Text",
                allowMultiple: false,
                strictType: true

            }




        ]
        // allowSelectBy: true

    });




    grist.onRecord(async (record, mappings) => {
        // console.log(await grist.fetchSelectedRecord(records[0].id, {includeColumns: "all", keepEncoded: true}));


        if (mappings) {
            _mappings = mappings;
            colTypesFetcher.gotMappings(mappings);
            const m2 = Object.assign({}, mappings);
            delete m2.contenu;
            mycols = Object.values(m2).concat(mappings.contenu);
        }



        mappedRecord = grist.mapColumnNames(record, mappings);
        // console.log('MAPPED', record, mappedRecord);

        // if any records were successfully mapped, create or update them in the calendar

        currentId = mappedRecord.id;
        let contents = mappedRecord.content.split(",");
        let sorts = mappedRecord.sort ? mappedRecord.sort.split(",") : [];

        sorts = sorts.map(s => {
            if (s.startsWith("<")) {
                return { key: s.substring(1), order: 1 }
            } else if (s.startsWith(">")) {
                return { key: s.substring(1), order: -1 }
            } else {
                return { key: s, order: -1 }
            }
        });
        // console.log("RECORD", mappedRecord, contents);

        // const colTypes = await colTypesFetcher.getColTypes();
        // const colOptions = await colTypesFetcher.getColOptions();
        // const couleur = colOptions[mappings.couleur];

        // let [,,groupe,couleur] = colOptions;

        rawtable = await grist.docApi.fetchTable(mappedRecord.table);
        // console.log('RAWTABLE', rawtable);

        colTypesFetcher.gotNewMappings(mappedRecord.table)

        metas = await colTypesFetcher.getColMeta();


        metas = metas.filter(m => m.colId == "manualSort" || m.colId.startsWith("gristHelper_") ? false : true);
        // console.log("--- METAS --- ", metas)

        filters = safeParse(mappedRecord.filters);
        if (filters === null) filters = {};

        if (!Object.keys(filters).includes(currentFilter)) {
            currentFilter = undefined;
        }



        restfilters = metas.filter(m => !Object.keys(filters).includes(m.colId)).map(m => '<option value="' + m.colId + '">' + m.label + '</option>').join("");
        let selection = [];

        div = document.getElementById("config");
        div.innerHTML = `<div><label for="filters">Filtres</label></div>
            <select name="filters" id="filters" onchange="editFilter()" multiple style="width:100%">`+
            metas.filter(m => Object.keys(filters).includes(m.colId)).map(m => '<option value="' + m.colId + '"' + (m.colId == currentFilter ? ' selected' : '') + '>' + m.label + '</option>').join("")
            + `</select>
            <div>
            <select id="addfilters" onchange="addFilters()" style="width:100%">
            <option value="">-- Ajouter --</option>`+ restfilters +
            `<select/>
            </div>`;


        editFilter();





        indexes = new Set(rawtable.id.map((v, i) => i));



        for (var key in filters) {
            let value = filters[key];
            rawtable[key].forEach((v, i) => { if (!value.include.includes(v)) indexes.delete(i) });
        }






        let metag = metas.find(m => m.colId == mappedRecord.groupe);
        let metasg = metas.find(m => m.colId == mappedRecord.sousGroupe);
        let groupeTable = {};
        let sousTable = {};

        if (metag.type.startsWith("Ref:")) {
            let table = metag.type.substring(4);
            let refa = await grist.docApi.fetchTable(table);
            let refmetas = await getTypes(table);
            let visibleCol = refmetas.find(rm => rm.id == metag.visibleCol);
            if (visibleCol !== undefined) {
                groupeTable = Object.fromEntries(
                    refa.id.map((id, idx) => [id, refa[visibleCol.colId][idx]])
                );
            }
        }

        if (metasg && metasg.type.startsWith("Ref:")) {
            let table = metasg.type.substring(4);
            let refa = await grist.docApi.fetchTable(table);
            let refmetas = await getTypes(table);
            let visibleCol = refmetas.find(rm => rm.id == metasg.visibleCol);
            if (visibleCol !== undefined) {
                sousTable = Object.fromEntries(
                    refa.id.map((id, idx) => [id, refa[visibleCol.colId][idx]])
                );

                // console.log("SOUS TABLE", sousTable);
            }
        }


        let groups = [], gidx = {};
        let minDate, maxDate;

        let choicemeta = metas.find(m => m.colId == mappedRecord.color)?.widgetOptions;

        if (choicemeta) {
            choicemeta = safeParse(choicemeta);
            if (choicemeta) {
                choicemeta = choicemeta.choiceOptions;
            }
        }

        indexes = [...indexes];

        if (sorts.length === 0) {
            indexes.sort((a, b) => {
                    return rawtable.manualSort[a] - rawtable.manualSort[b]
            });
        } else {
            indexes.sort((a, b) => {
                return sorts.reduce((acc, sort) => {
                    if (acc === 0) {
                        let x = rawtable[sort.key][a], y = rawtable[sort.key][b];

                        if (typeof x === typeof y) {
                            switch (typeof x) {
                                case "number":
                                    return x - y;
                                    break;
                                case "string":
                                    x.localeCompare(y);
                                    break;
                            }
                            return acc;

                        } else {
                            return acc;
                        }
                    } else {
                        return acc;
                    }

                }, 0)
            });

        }

        indexes.forEach(idx => {


            let gid = rawtable[mappedRecord.groupe][idx];
            let glabel = groupeTable[gid] || gid;
            let start = rawtable[mappedRecord.fromField][idx] * 1000;
            let end = rawtable[mappedRecord.toField][idx] * 1000;

            if (mappedRecord.sousGroupe) {
                let sgid = rawtable[mappedRecord.sousGroupe][idx];
                gid = gid + " : " + sgid;
                glabel = [glabel, sousTable[sgid] || sgid];
            }

            let group = gidx[gid] !== undefined ? groups[gidx[gid]] : { label: glabel, sections: [] }
            if (gidx[gid] == undefined) {
                gidx[gid] = groups.length;
                groups.push(group);

            }





            group.sections.push({
                label: contents.map(key => rawtable[key][idx] || ""),
                start: start,
                end: end,
                // color: "grey"
                color: choicemeta ? choicemeta[rawtable[mappedRecord.color][idx]]?.fillColor || "grey" : "grey"
            });

            if (minDate === undefined || start < minDate) {
                minDate = start;
            }

            if (maxDate === undefined || end > maxDate) {
                maxDate = end;
            }



        });

        let from = new Date(mappedRecord.fromDate ? mappedRecord.fromDate : minDate);
        let to = new Date(mappedRecord.toDate ? mappedRecord.toDate : maxDate);

        // console.log("FROM TO", mappedRecord.fromDate, mappedRecord.toDate);

        // console.log("GROUPS", new Date(from), new Date(to), groups);
        config = {
            title: mappedRecord.title
            , fromDate: from
            , toDate: to
            , vpages: mappedRecord.vpages
            , hpages: mappedRecord.hpages
            , groupsFontSize: mappedRecord.groupsFontSize // points
            , tasksFontSize: mappedRecord.tasksFontSize // points
            , hoursFontSize: mappedRecord.hoursFontSize // points
            , mode: mappedRecord.mode === "Horizontal" ? 0 : 1 // Horiz, Vert
            , orientation: mappedRecord.orientation === "Horizontal" ? 0 : 1 // portrait, paysag   e
            , align: mappedRecord.align === "Gauche" ? "left" : (mappedRecord.align === "Centre" ? "center" : "right")
            , multiline: mappedRecord.multiline
            , papersize: mappedRecord.papersize
        }


        // console.log(generate(groups, config));

        generateTimelinePdf(mappedRecord.title, groups, config);

    });

    grist.onOptions(opts => {
        if (!options) {
            options = opts ? opts : undefined;
        }
    })


});


// We have no good way yet to get the type of a mapped column when multiple types are allowed. We
// get it via the metadata tables instead. There is no good way to know when a column's type is
// changed, so we skip that for now.
// TODO: Drop all this once the API can tell us column info.$


async function getTypes(tableId) {
    const tables = await grist.docApi.fetchTable('_grist_Tables');
    const columns = await grist.docApi.fetchTable('_grist_Tables_column');
    const fields = Object.keys(columns);
    const tableRef = tables.id[tables.tableId.indexOf(tableId)];


    const colIndexes = columns.parentId.map((id, i) => [id, i]).filter(item => item[0] === tableRef).map(item => item[1]);


    const types = colIndexes.map(index => {
        return Object.fromEntries(fields.map(f => [f, columns[f][index]]));
    });

    return types;
}

class ColTypesFetcher {
    // Returns array of column records for the array of colIds.


    constructor() {
        this._tableId = null;
        this._colIds = null;
        this._colTypesPromise = Promise.resolve([null, null]);
        this._accessLevel = 'full';
    }
    setAccessLevel(accessLevel) {
        this._accessLevel = accessLevel;
    }
    gotMappings(mappings) {
        // Can't fetch metadata when no full access.
        if (this._accessLevel !== 'full') { return; }
        const m2 = Object.assign({}, mappings);
        delete m2.contenu;
        const flat = Object.values(m2).concat(mappings.contenu);
        if (!this._colIds || !(this._colIds.toString() === flat.toString())) {
            this._colIds = flat;
            if (this._tableId) {
                this._colTypesPromise = getTypes(this._tableId);
            }
        }
    }
    gotNewMappings(tableId) {
        // Can't fetch metadata when no full access.
        if (this._accessLevel !== 'full') { return; }
        this._tableId = tableId;
        if (this._colIds) {
            this._colTypesPromise = getTypes(this._tableId);
        }
    }

    async getColTypes() {
        return this._colTypesPromise.then(types => types.map(t => t?.type));
    }

    async getColOptions() {
        // this._colTypesPromise.then((tp) => console.log("to",tp));
        return this._colTypesPromise.then(
            types => Object.fromEntries(types.map(t => [t.colId, safeParse(t?.widgetOptions)]))
        );
    }

    async getColMeta() {
        return this._colTypesPromise;
    }
}


function safeParse(value) {
    try {
        return JSON.parse(value);
    } catch (err) {
        return null;
    }
}

const colTypesFetcher = new ColTypesFetcher();

