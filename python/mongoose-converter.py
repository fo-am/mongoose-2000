#!/usr/bin/env python
import sqlite3
import sys
import platform;

if int(platform.python_version_tuple()[0])>2:
	from tkinter import *
	from tkinter.filedialog import *
	from tkinter.messagebox import *
else:
	from Tkinter import *
	from tkFileDialog import *
	from tkMessageBox import *

version = "0.0.3"

def msg(msg):
    print(msg)

def clear_msg():
    pass


class ktv:
    def __init__(self,key,type,value):
        self.key = key
        self.type = type
        self.value = value

    def pretty_print(self):
        print(self.key+":"+self.type+"="+str(self.value))

def ktv_get(ktv_list,key):
    for i in ktv_list:
        if i.key==key: return i.value
    return False

def get_attribute_ids_types(db, table, entity_type):
    c = db.cursor()
    c.execute('select attribute_id, attribute_type from '+table+'_attribute where entity_type=?', (entity_type,))
    return c.fetchall()

def get_all_entity_types(db, table):
    c = db.cursor()
    c.execute('select distinct entity_type from '+table+'_entity;')
    return map(lambda a: a[0], c.fetchall())

def get_entity_type(db, table, entity_id):
    c = db.cursor()
    c.execute('select entity_type from '+table+'_entity where entity_id = ?',
              (entity_id,))
    t=c.fetchone()
    if t: return t[0]
    else: return False

def get_value(db, table, entity_id, kt):
    c = db.cursor()
    c.execute('select value, dirty from '+table+'_value_'+kt.type+
              ' where entity_id = ? and attribute_id = ?',
              (entity_id, kt.key))
    return c.fetchone()

def safe_build_ktv(key,type,v):
    if v:
        return ktv(key, type, v[0])
    else:
        return ktv(key, type, -1)


def get_entity_plain(db, table, entity_id):
    entity_type = get_entity_type(db,table,entity_id)
    #print entity_id
    #print entity_type
    return map(lambda idtype:
               safe_build_ktv(idtype[0], idtype[1], get_value(db,table,entity_id,ktv(idtype[0],idtype[1],888))),
               get_attribute_ids_types(db,table,entity_type))

def get_unique_id(db, table, entity_id):
    c = db.cursor()
    c.execute('select unique_id from '+table+'_entity where entity_id = ?',
              (entity_id,))
    t = c.fetchone()
    if t: return t[0]
    else: return False

def get_entity(db, table, entity_id):
  #print get_unique_id(db, table, entity_id)
  return [ktv("unique_id", "varchar", get_unique_id(db, table, entity_id))] +\
      get_entity_plain(db, table, entity_id)

def all_entities(db, table, entity_type):
    c = db.cursor()
    c.execute('select e.entity_id from '+table+'_entity as e '\
              'join '+table+'_value_varchar '\
              ' as n on n.entity_id = e.entity_id and n.attribute_id = ? '\
              'left join '+table+'_value_int '\
              'as d on d.entity_id = e.entity_id and d.attribute_id = ? '\
              'where e.entity_type = ? '\
              'and (d.value=\'NULL\' or d.value is NULL or d.value = 0) '\
              'order by n.value',
              ('name', 'deleted', entity_type,))
    return c.fetchall()

def get_entity_id(db, table, unique_id):
    c = db.cursor()
    c.execute('select entity_id from '+table+'_entity where unique_id = ?',
              (unique_id,))
    t = c.fetchone()
    if t: return t[0]
    else: return False

# cache em
uid_to_entity = {}

def get_entity_by_unique(db, table, unique_id):
    if unique_id in uid_to_entity:
        return uid_to_entity[unique_id]
    e = get_entity(db, table, get_entity_id(db, table, unique_id))
    uid_to_entity[unique_id]=e
    return e

# cache em
uid_to_name = {}

def get_entity_name(db, table, unique_id):
    if unique_id in uid_to_name:
        return uid_to_name[unique_id]

    if unique_id=="Unknown":
            name="Unknown"
    elif unique_id=="None":
            name="None"
    else:
            name = ktv_get(get_entity_by_unique(db, table, unique_id), "name")
            uid_to_name[unique_id]=name
    return name


def get_entity_names(db, table, id_list):
    def _(r,uid):
        name = get_entity_name(db, table, uid)
        if name and name!=-1 : return r+name+":"
        else:
            msg("could not find entity: "+uid)
            return r+"Name not found:"

    if id_list==-1 or id_list=="NULL" or id_list=="": return ""
    return reduce(_,id_list.split(","), "")

def csv_titles(db, table, entity_type):
    return reduce(lambda r,kt:
                  r+"\""+kt[0]+"\",",
                  get_attribute_ids_types(db, table, entity_type),
                  "")

def write_csv(f, db, table, entity_type, raw):
    count=0
    c = db.cursor()
    c.execute('select e.entity_id from '+table+'_entity as e '\
              'where e.entity_type = ?',
              (entity_type,))
    records = c.fetchall()
    msg("converting "+str(len(records))+" records")
    f.write(csv_titles(db,table,entity_type)+"\n")
    for n,record in enumerate(records):
        msg(entity_type+": "+str(n)+" out of "+str(len(records))+(": %02d"%(n/float(len(records))*100.0))+"% done...")
        entity_id = record[0]
        f.write(conv_csv(db,get_entity_plain(db, table, entity_id))+"\n")
        #f.write(conv_csv_raw(db,get_entity_plain(db, table, entity_id))+"\n")

def conv_csv_ktv(db,k):
    if k==None:
        return ""
    elif k.key[0:8]=="id-list-" or\
         k.key=="pregnant" or\
         k.key=="present" or\
         k.key=="baby-byelim" or\
         k.key=="baby-seen":
        if type(k.value) is int: return str(k.value)

        names = get_entity_names(db,"sync",k.value)
        if not names:
            return "Name not found"
        else:
            return "\""+names+"\""
    elif k.key[0:3]=="id-" or k.key=="pack":
        name = get_entity_name(db,"sync",k.value)
        if not name or name==-1:
            print("name not found for "+str(k.value))
            return "Name not found"
        else:
            return "\""+name+"\""
    else:
        if type(k.value)=="String":
            return "\""+k.value+"\""
        else: return str(k.value)

def conv_csv(db,data):
    return reduce(lambda r,elem:
                  r+conv_csv_ktv(db,elem)+", ",
                  data, "")

def conv_csv_raw(db, data):
    return reduce(lambda r,elem:
                  r+"\""+str(elem.value)+"\", ",
                  data, "")

############################################################

class win:
    def __init__(self):
        self.db = False
        self.table = "stream"

        # create window
        self.root = Tk()
        self.root.title("mongoose converter "+version)
        top = Frame(self.root)
        top.pack(fill=BOTH)

        f=Frame(top)
        f.pack(side=LEFT);
        Button(f, text="load database", command=self.load_database).grid(row=0, column=0, sticky="we")
        Button(f, text="extract csv to", command=self.save_as).grid(row=0, column=1, sticky="we")

        self.pack_var = IntVar()
        cb=Checkbutton(f, text="pack data", variable=self.pack_var, command=self.pack)
        cb.grid(row=0, column=2)

        self.debug = Text(self.root, height=30, width=60)
        self.debug.pack()
        self.debug.insert(END, "ready for action...\n")



    def msg(self,msg):
        self.debug.insert(END, msg+"\n")
        self.debug.see(END)
        self.root.update()

    def clear_msg(self):
        self.debug.delete(0.0, END)
        self.root.update()

    def pack(self):
        clear_msg()
        if self.pack_var.get()==1:
            msg("switching to pack data")
            self.table="sync"
            self.print_types()
        else:
            msg("switching to observation data")
            self.table="stream"
            self.print_types()

    def print_types(self):
        msg("record types available are:")
        for i in get_all_entity_types(self.db,self.table):
            msg(i)

    def load_database(self):
        filename = askopenfilename(title = "load database")
        if filename!="":
            self.clear_msg()
            self.db = sqlite3.connect(filename)
            table = 'stream'
            msg("opened "+filename)
            self.print_types()
            #print get_attribute_ids_types(db, 'stream', 'group-interaction')

    def save_as(self):
        filename = askdirectory(title = "choose a directory to save the csv files in")
        if filename!="":
            for i in get_all_entity_types(self.db,self.table):
                msg("writing "+i)
                with open(filename+"/"+i+'.csv','w') as f:
                    write_csv(f,self.db,self.table,i,False)


# precache old mongoose names
#fallback_fn = "mongoose-2015-09-30 20:48:57.db"
#fallback_db = sqlite3.connect(fallback_fn)
#for e in all_entities(fallback_db, "sync", "mongoose"):
#    print(get_entity_name(fallback_db, "sync", get_unique_id(fallback_db,"sync",e[0])))


w = win()

msg = w.msg
clear_msg = w.clear_msg

try:
    w.root.mainloop()
except Exception,e:
    msg(e)
