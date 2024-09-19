const secondsPerDay = 24 * 60 * 60;

const params = new URLSearchParams(document.location.search);
const paramEndDate = params.get("mode") == "fin";

let _mappings;

window.addEventListener('load', (event) => {
  console.log('La page est complètement chargée');

  let mappedRecords = [];
  let newSelection;
  let options = undefined;
  let rawtable;







  // const seedFlag = [randInts[0], randInts.slice(1)]
  var app = Elm.Widget.init();

  app.ports.updateOptions.subscribe(opts => {
    options = opts;

    grist.setOptions(opts);
  })


  app.ports.modifyRecords.subscribe(async change => {

    console.log("MODIFY", change);
    try {
      const colTypes = await colTypesFetcher.getColTypes();
      const isFormula = await colTypesFetcher.getColIsFormula();

      updateGristRecords(
        change.ids.map(id => {
          const rec = mappedRecords.find(r => r.id == id);

          if (rec) {

            if (change.setDuree !== undefined) {
              if (paramEndDate) {
                const newEnd = (new Date(rec.date));
                newEnd.setSeconds(newEnd.getSeconds() + change.setDuree);
                return {
                  id,
                  fin: newEnd
                }

              } else {
                return {
                  id,
                  duree: (change.setDuree / 3600)
                }

              }

            } else if (change.setFin !== undefined) {
              if (paramEndDate) {
                const newEnd = (new Date(change.setFin));

                return {
                  id,
                  fin: newEnd
                }

              } else {

                return {
                  id,
                  duree: ((new Date(change.setFin)).valueOf() - (new Date(rec.date)).valueOf()) / 3600000
                }

              }

            } else if (change.changeDebut !== undefined) {
              const newDate = (new Date(rec.date));
              newDate.setSeconds(rec.date.getSeconds() + change.changeDebut);

              if (paramEndDate) {
                const newEnd = (new Date(rec.fin));
                newEnd.setSeconds(rec.fin.getSeconds() + change.changeDebut + change.changeAmplitude);

                return {
                  id,
                  date: newDate,
                  fin: newEnd
                }

              } else {
                return {
                  id,
                  date: newDate,
                  duree: Math.max(1, (rec.duree ? rec.duree : 7) + (change.changeAmplitude / 3600))
                }

              }

            } else if (change.groupeId !== undefined) {

              const groupe = colTypes[_mappings.groupe].startsWith("Ref:") ? parseInt(change.groupeId) : change.groupeId;

              const o = { id };

              if (change.sousGroupeId && !isFormula[_mappings.sousGroupe]) {
                const sousGroupe = colTypes[_mappings.sousGroupe].startsWith("Ref:") ? parseInt(change.sousGroupeId) : change.sousGroupeId;
                o.sousGroupe = sousGroupe;
              }

              if (!isFormula[_mappings.groupe]) {
                o.groupe = groupe;
              }

              return o;


            }




          }


        })
      );
    } catch (err) {
      console.error(err);
    }
  }
  );

  app.ports.updateField.subscribe(async change => {
    try {
      const table = await grist.getTable();
      console.log("UPDATE FIELD", change);

      await table.update(
        change.ids.map(id => {

          const o = { id };
          o.fields = {};
          o.fields[change.field] = change.value;
          // console.log("UPD FIELD "+id, o);
          return o;

        })
      );
    } catch (err) {
      console.error(err);
    }
  }
  );


  app.ports.createRecord.subscribe(async rec => {
    try {
      const colTypes = await colTypesFetcher.getColTypes();
      const isFormula = await colTypesFetcher.getColIsFormula();

      rec.date = new Date(rec.date);
      if (paramEndDate) {
        rec.fin = new Date(rec.date);
        rec.fin.setSeconds(rec.fin.getSeconds() + rec.duree);
        delete rec.duree;
      } else {
        rec.duree = rec.duree / 3600;
      }

      if (!isFormula[_mappings.groupe]) {
        if (colTypes[_mappings.groupe].startsWith("Ref:")) {
          rec.groupe = parseInt(rec.groupeId);
        } else {
          rec.groupe = rec.groupeId;
        }
      }

      if (!isFormula[_mappings.sousGroupe]) {
        if (rec.sousGroupeId && colTypes[_mappings.sousGroupe].startsWith("Ref:")) {
          rec.sousGroupe = parseInt(rec.sousGroupeId);
        } else if (rec.sousGroupeId) {
          rec.sousGroupe = rec.sousGroupeId;
        }

      }

      delete rec.groupeId;
      delete rec.sousGroupeId;

      let id = await upsertGristRecord(rec);
      newSelection = id ? [{ id }] : undefined;
    } catch (err) {
      console.error(err);
    }
  }
  );

  app.ports.selectRecords.subscribe(sel => {
    try {
      grist.setSelectedRows(sel);
      newSelection = sel;
    } catch (err) {
      // Nothing clever we can do here, just log the error.
      // Grist should actually show the error in the UI, but it doesn't.
      console.error(err);
    }
  }
  );

  app.ports.deleteRecords.subscribe(async del => {
    try {
      const table = await grist.getTable();
      await table.destroy(del.ids);
    } catch (err) {
      console.error(err);
    }
  }
  );

  app.ports.cloneRecords.subscribe(async change => {
    try {

      const t = rawtable;
      const colMeta = await colTypesFetcher.getColMeta();
      const colTypes = await colTypesFetcher.getColTypes();
      const isFormula = await colTypesFetcher.getColIsFormula();

      let eventsInValidFormat = [];
      for (const id of change.ids) {
        let rec = mappedRecords.find(r => r.id == id);

        if (rec) {
          const newDate = (new Date(rec.date));
          newDate.setSeconds(rec.date.getSeconds() + change.changeDebut);

          let evt = {};

          if (change.changeDebut !== 0) {
            if (paramEndDate) {
              const newEnd = new Date(rec.fin);
              newEnd.setSeconds(rec.fin.getSeconds() + change.changeDebut);

              evt = {
                date: newDate,
                fin: newEnd
              }
            }
            else {
              evt = { date: newDate }
            }


          } else {

            if (!isFormula[_mappings.groupe] && (change.groupeId !== "")) {
              if (colTypes[_mappings.groupe].startsWith("Ref:")) {
                evt.groupe = parseInt(change.groupeId);
              } else {
                evt.groupe = evt.groupeId;
              }
            }

            if (!isFormula[_mappings.sousGroupe] && (change.sousGroupeId !== "")) {
              if (colTypes[_mappings.sousGroupe].startsWith("Ref:")) {
                evt.sousGroupe = parseInt(change.sousGroupeId);
              } else {
                evt.sousGroupe = evt.sousGroupeId;
              }
            }




          }


          evt = makeEventInValidFormat(evt);

          const index = t.id.indexOf(id);

          if (index === -1) return;

          const cols = colMeta.filter(col => !(col.colId === "manualSort") && (col.isFormula === false)).map(col => col.colId);
          let orig = Object.fromEntries(cols.map(col => [col, t[col][index]]));
          // let orig = await grist.fetchSelectedRecord(id, {format: "rows", includeColumns: "all", keepEncoded: false});

          if(!evt) {
            evt = {};
          }

          if (evt.fields) {
            for (var prop in evt.fields) {
              orig[prop] = evt.fields[prop];
            }
          }

          evt.fields = orig;
          // delete evt.id;

          eventsInValidFormat.push(evt);

        }


      };

      // console.log("clone", eventsInValidFormat);
      // console.log(await grist.docApi.fetchTable(await grist.selectedTable.getTableId()));
      const table = await grist.getTable();
      newSelection = await table.create(eventsInValidFormat);



    } catch (err) {
      // Nothing clever we can do here, just log the error.
      // Grist should actually show the error in the UI, but it doesn't.
      console.error(err);
    }
  }
  );

  app.ports.splitRecords.subscribe(async change => {
    try {

      const t = rawtable;
      const colMeta = await colTypesFetcher.getColMeta();
      let updateR = [], createR = [];
      for (const id of change.ids) {
        let rec = mappedRecords.find(r => r.id == id);

        if (rec && (change.date)) {
          const splitDate = new Date(change.date);
          const origDate = new Date(rec.date);
          let rightEvt, leftEvt;

          if (paramEndDate) {
            const endDate = new Date(rec.fin);
            if (splitDate < origDate || splitDate > endDate) continue;
            rightEvt = { date: splitDate, fin: endDate };
            leftEvt = { id: id, fin: splitDate }
          } else {

            const leftD = (splitDate - origDate) / 3600000;
            const rightD = rec.duree - leftD;
            if (!(leftD > 0 && rightD > 0)) continue;
            rightEvt = { date: splitDate, duree: rightD };
            leftEvt = { id: id, duree: leftD }
          }

          rightEvt = makeEventInValidFormat(rightEvt);

          const index = t.id.indexOf(id);
          if (index === -1) continue;

          const cols = colMeta.filter(col => !(col.colId === "manualSort") && (col.isFormula === false)).map(col => col.colId);
          let orig = Object.fromEntries(cols.map(col => [col, t[col][index]]));

          for (var prop in rightEvt.fields) {
            orig[prop] = rightEvt.fields[prop];
          }

          rightEvt.fields = orig;

          createR.push(rightEvt);
          updateR.push(makeEventInValidFormat(leftEvt));
        }
      };

      // console.log("split", updateR, createR);
      const table = await grist.getTable();
      await table.update(updateR);
      newSelection = await table.create(createR);

    } catch (err) {
      // Nothing clever we can do here, just log the error.
      // Grist should actually show the error in the UI, but it doesn't.
      console.error(err);
    }
  }
  );


  grist.ready({
    requiredAccess: 'full',
    allowSelectBy: true,
    columns: [
      {
        name: "date", // What field we will read.
        title: "Date et heure", // Friendly field name.
        optional: false, // Is this an optional field.
        type: "DateTime", // What type of column we expect.
        //   description: "D", // Description of a field.
        allowMultiple: false, // Allows multiple column assignment.
        strictType: true
      },
      paramEndDate ?
        {
          name: "fin",
          title: "Fin",
          optional: false,
          type: "DateTime",
          //   description: "D",
          allowMultiple: false,
          strictType: true
        }
        :
        {
          name: "duree",
          title: "Durée",
          optional: false,
          type: "Numeric, Int",
          //   description: "D",
          allowMultiple: false,
          strictType: true
        },
      {
        name: "groupe",
        title: "Grouper par",
        optional: false,
        type: "Any",
        //   description: "D",
        allowMultiple: false
      },
      {
        name: "sousGroupe",
        title: "Puis grouper par",
        optional: true,
        type: "Any",
        //   description: "D",
        allowMultiple: false
      },

      {
        name: "couleur",
        title: "Couleur",
        optional: true,
        type: "Choice",

      },

      {
        name: "commentaire",
        title: "Commentaire",
        optional: true,
        type: "Any",

      },

      {
        name: "contenu",
        title: "Contenu",
        optional: true,
        type: "Any",
        // type: "Text, Int, Numeric",
        //   description: "D",
        allowMultiple: true,
        // strictType: true
      },

      {
        name: "fields",
        title: "Champs éditables",
        optional: true,
        type: "Any",
        // type: "Text, Int, Numeric",
        //   description: "D",
        allowMultiple: true,
        // strictType: true
      },

    ]
  });

  grist.onRecords(async (records, mappings) => {
    // console.log(await grist.fetchSelectedRecord(records[0].id, {includeColumns: "all", keepEncoded: true}));

    if (mappings) {
      _mappings = mappings;
      colTypesFetcher.gotMappings(mappings);
      const m2 = Object.assign({}, mappings);
      delete m2.contenu;
    }



    mappedRecords = grist.mapColumnNames(records, mappings);
    console.log("MAPPINGS", mappings);
    // if any records were successfully mapped, create or update them in the calendar
    if (mappedRecords) {
      // const colTypes = await colTypesFetcher.getColTypes();
      const colOptions = await colTypesFetcher.getColOptions();
      const couleur = colOptions[mappings.couleur];

      // let [,,groupe,couleur] = colOptions;
      // let choice;
      let oneid;
      rawtable = await grist.selectedTable.getTableId().then(id => grist.docApi.fetchTable(id));

      const data = mappedRecords.map(rec => {

        const clone = Object.assign({}, rec)

        clone.date = (new Date(clone.date.valueOf())).toISOString();

        if (paramEndDate) {
          clone.duree = (rec.fin.valueOf() - rec.date.valueOf()) / 3600000;
        }
        clone.couleur = couleur?.choiceOptions?.[clone.couleur]?.fillColor;
        clone.groupeId = rawtable[mappings.groupe][rawtable.id.indexOf(rec.id)];
        clone.groupeId = clone.groupeId ? clone.groupeId + "" : "";
        if (mappings.sousGroupe) {
          clone.sousGroupeId = rawtable[mappings.sousGroupe][rawtable.id.indexOf(rec.id)];
          clone.sousGroupeId = clone.sousGroupeId ? clone.sousGroupeId + "" : "";

        }
        clone.fields = Object.fromEntries(mappings.fields.map((key, idx) => [key, clone.fields[idx]]));
        // console.log("FIELDS", clone.fields);

        oneid = rec.id;

        return clone;
      });


      // grist.fetchSelectedRecord(oneid, {keepEncoded: true, includeColumns: "normal"}).then(rec => console.log("rec", rec));
      // grist.selectedTable.getTableId().then(id => grist.docApi.fetchTable(id)).then(rec => console.log("rec", rec));



      const colMeta = await colTypesFetcher.getColMeta();
      const editableTypes_ = mappings.fields.map(m => colMeta.find(cm => cm.colId === m && cm.isFormula === false))
        .filter(v => v !== undefined)
      const editableTypes = await Promise.all(
        editableTypes_.map(async f => {

          if (f.type.startsWith("Ref:")) {
            try {
              const table = await grist.docApi.fetchTable(f.type.substring(4));
              const columns = await grist.docApi.fetchTable('_grist_Tables_column');
              const index = columns.id.indexOf(f.visibleCol);
              // console.log("TABLE", table);
              // console.log("COLS", columns, f.visibleCol, index);

              if (index > -1) {
                col = columns.colId[index];
                f.references = table.id.map((id, idx) => ({ id, label: table[col][idx] }));
              }
              return f;

            } catch (err) {
              console.log("ERRORERROR", err);
              return f;
            }

          } else
            return f;
        }));
      console.log("EDITABLE", editableTypes);
      app.ports.setRecords.send(newSelection ? { rows: data, selection: newSelection, editable: editableTypes } : { rows: data, editable: editableTypes });
      if (newSelection) grist.setSelectedRows(newSelection);
      // newSelection = undefined;
    }


    // const events = mappedRecords
    //     .filter(isRecordValid)
    //     .map(r => buildCalendarEventObject(r, colTypes, colOptions));
    // calendarHandler.setEvents(new Map(events.map(event => ([event.id, event]))));
    // updateUIAfterNavigation();
    // }




  });

  grist.onOptions(opts => {
    if (!options) {
      options = opts ? opts : undefined;
      if (options) app.ports.setOptions.send(options);
    }
  })

  grist.on('message', (e) => {
    if (e.tableId && e.mappingsChange) { colTypesFetcher.gotNewMappings(e.tableId); }
  });

});


// We have no good way yet to get the type of a mapped column when multiple types are allowed. We
// get it via the metadata tables instead. There is no good way to know when a column's type is
// changed, so we skip that for now.
// TODO: Drop all this once the API can tell us column info.
class ColTypesFetcher {
  // Returns array of column records for the array of colIds.
  static async getTypes(tableId, colIds) {
    const tables = await grist.docApi.fetchTable('_grist_Tables');
    const columns = await grist.docApi.fetchTable('_grist_Tables_column');
    const fields = Object.keys(columns);
    const tableRef = tables.id[tables.tableId.indexOf(tableId)];



    const colIndexes = columns.parentId.map((id, i) => [id, i]).filter(item => item[0] === tableRef).map(item => item[1]);


    const types = colIndexes.map(index => {
      // console.log(fields.map(f => [f, columns[f][index]]), Object.fromEntries(fields.map(f => [f, columns[f][index]])));
      let t = Object.fromEntries(fields.map(f => [f, columns[f][index]]));
      t.widgetOptions = safeParse(t.widgetOptions);
      return t;
    });


    // const types= colIds.map(colId => {
    //   const index = columns.id.findIndex((id, i) => (columns.parentId[i] === tableRef && columns.colId[i] === colId));
    //   if (index === -1) { return null; }
    //   return Object.fromEntries(fields.map(f => [f, columns[f][index]]));
    // });

    // console.log("types", JSON.stringify(types));
    console.log("types", types);
    return types;
  }

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
        this._colTypesPromise = ColTypesFetcher.getTypes(this._tableId, this._colIds);
      }
    }
  }
  gotNewMappings(tableId) {
    // Can't fetch metadata when no full access.
    if (this._accessLevel !== 'full') { return; }
    this._tableId = tableId;
    if (this._colIds) {
      this._colTypesPromise = ColTypesFetcher.getTypes(this._tableId, this._colIds);
    }
  }

  async getColTypes() {
    return this._colTypesPromise.then(
      types => Object.fromEntries(types.map(t => [t.colId, t?.type]))
    );
  }

  async getColOptions() {
    // this._colTypesPromise.then((tp) => console.log("to",tp));
    return this._colTypesPromise.then(
      types => Object.fromEntries(types.map(t => [t.colId, t?.widgetOptions]))
    );
  }

  async getColIsFormula() {
    return this._colTypesPromise.then(
      types => Object.fromEntries(types.map(t => [t.colId, t?.isFormula]))
    );
  }

  async getColMeta(colid) {
    return this._colTypesPromise;
  }
}

const colTypesFetcher = new ColTypesFetcher();



function safeParse(value) {
  try {
    return JSON.parse(value);
  } catch (err) {
    return null;
  }
}


async function upsertGristRecord(gristEvent) {
  try {

    const eventInValidFormat = makeEventInValidFormat(gristEvent);
    const table = await grist.getTable();


    if (gristEvent.id) {
      // console.log("upsertGristRecord", eventInValidFormat);

      await table.update(eventInValidFormat);
    } else {
      const { id } = await table.create(eventInValidFormat);
      await grist.setCursorPos({ rowId: id });
      return id;
    }
  } catch (err) {
    // Nothing clever we can do here, just log the error.
    // Grist should actually show the error in the UI, but it doesn't.
    console.error(err);

  }
}

async function updateGristRecords(gristEvents) {
  try {
    eventsInValidFormat = gristEvents.map(ev => makeEventInValidFormat(ev));
    const table = await grist.getTable();
    console.log("updateGristRecords", eventsInValidFormat);

    await table.update(eventsInValidFormat);

  } catch (err) {
    // Nothing clever we can do here, just log the error.
    // Grist should actually show the error in the UI, but it doesn't.
    console.error(err);

  }
}

function makeEventInValidFormat(gristEvent) {
  if (!_mappings) { return; }

  id = gristEvent.id;
  delete gristEvent.id;

  const filteredRecord = Object.fromEntries(Object.entries(gristEvent)
    .map(([key, value]) => [_mappings[key], value]));
  // console.log("makeEventInValidFormat MAPPED", filteredRecord);
  // Send nothing if there are no changes.
  if (Object.keys(filteredRecord).length === 0) { return; }
  return { id, fields: filteredRecord };

}


async function deleteEvent(event) {
  try {
    const table = await grist.getTable();
    await table.destroy(event.id);
  } catch (e) {
    console.error(e);
  }
}