
# coding: utf-8

# In[35]:

from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas
import pandas as pd
import os
import sys
import argparse

# add custom modules directory
sys.path.append('/home/j/WORK/10_gbd/00_library/functions')
sys.path.append('J:/WORK/10_gbd/00_library/functions')

# import get_prefix as pfx

#### parse arguments
parser = argparse.ArgumentParser()
parser.add_argument("--outdir", help="output file", required=True)
parser.add_argument("--title", help="title", required=True)
parser.add_argument("--daly_v", help="dalynator version", required=True)
parser.add_argument("--year1", help="start year", required=True)
parser.add_argument("--year2", help="end year", required=True)
args = parser.parse_args()

# change default path
# prefix = pfx.get_prefix()
os.chdir('{outdir}'.format(outdir=args.outdir))

#### import datafrom
df = pd.read_csv(r"/homes/eeldren/chn_practicum_code/arrowset_double_ages_98_99_GBD2016.csv")


#### open file for writing
file = '{outdir}/arrowset_double_ages_98_99_GBD2016_v{daly_v}.pdf'.format(outdir=args.outdir,daly_v=args.daly_v)
c = canvas.Canvas(file, pagesize=letter)


#### text size and style
titletextsize = 8
textsize = 7.5
textgap = textsize*1.5


#### write title
titley = 770
titlex = 300
row1 = titley-(2*textgap)
row2 = row1-(0.75*textgap)
c.setFont("Helvetica-Bold",titletextsize)
c.drawString(titlex-(c.stringWidth(
        '{title}'.format(title=args.title))/2),
        titley,
        '{title}'.format(title=args.title))


#### write column headers
c.setFont("Helvetica-Bold",textsize)
columnwidth1 = 135
columnwidth2 = 135
columnwidth3 = 75
gap = 65

# set columns widths (counting from left to right)
column1 = 30
column2 = column1 + columnwidth1 + 150
column3 = column2 + columnwidth2 + 15

# name columns
c.drawString(column1,row2,'Leading causes {year1}'.format(year1=args.year1))
c.drawString(column2,row2,'Leading causes {year2}'.format(year2=args.year2))


#### set dictionary for fill colors
fill = {
        'A':[.7490196,.2470588,.2470588],
        'B':[.2470588,.2470588,.7490196],
        'C':[.2470588,.7490196,.2470588]
        }


#### loop through non residuals
c.setFont("Helvetica",textsize)
iter = 1
for i in range(len(df[df.resid != 1])):
    # write text
    c.setStrokeColorRGB(0,0,0)
    c.setFillColorRGB(0,0,0)
    c.setStrokeAlpha(1)
    c.setFillAlpha(1)
    c.drawString(column1+1,row2-(iter*textgap),'{row_id_year1} {cause_medium_year1}'.format(cause_medium_year1=df.cause_medium_year1.ix[i],row_id_year1=df.row_id_year1.ix[i]))
    c.drawString(column2+1,row2-(iter*textgap),'{row_id_year2} {cause_medium_year2}'.format(cause_medium_year2=df.cause_medium_year2.ix[i],row_id_year2=df.row_id_year2.ix[i]))
    
    # set color for start year based on cause_type
    c.setFillColorRGB(fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][0],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][1],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][2])
    c.setFillAlpha(0.2)
    c.setStrokeColorRGB(fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][0],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][1],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][2])
    c.setStrokeAlpha(0.7)
            
    # fill with color start year
    c.setDash(1,0)
    c.rect(column1,row2-(iter*textgap)-2.5,columnwidth1,textsize*1.3,stroke=1,fill=1)
    
    #determine rank change
    num_start = df[df.cause_medium_year1 == df.cause_medium_year1.ix[i]].index.values[0]
    num_end = df[df.cause_medium_year2 == df.cause_medium_year1.ix[i]].index.values[0]
    
    # detemine line type
    if num_start >= num_end:
        c.setDash(1,0)
    else:
        c.setDash(3,1)

    #draw line
    c.line(column1+columnwidth1,row2-((num_start+1)*textgap)+(.33*textsize),column2,row2-((num_end+1)*textgap)+(.33*textsize))

    # set color for end year based on cause_type
    c.setFillColorRGB(fill['{cause_year2}'.format(cause_year2 = df.cause_year2.ix[i])][0],fill['{cause_year2}'.format(cause_year2 = df.cause_year2.ix[i])][1],fill['{cause_year2}'.format(cause_year2 = df.cause_year2.ix[i])][2])
    c.setFillAlpha(0.2)
    c.setStrokeColorRGB(fill['{cause_year2}'.format(cause_year2 = df.cause_year2.ix[i])][0],fill['{cause_year2}'.format(cause_year2 = df.cause_year2.ix[i])][1],fill['{cause_year2}'.format(cause_year2 = df.cause_year2.ix[i])][2])
    c.setStrokeAlpha(0.7)

    # fill with color end year
    c.setDash(1,0)
    c.rect(column2,row2-(iter*textgap)-2.5,columnwidth2,textsize*1.3,stroke=1,fill=1)
   
    # iterate
    iter = iter + 1
   
# store column iterations
legend_iter = iter

#### loop through resids
for i in range((iter-1),len(df)):
    # write text
    c.setStrokeColorRGB(0,0,0)
    c.setFillColorRGB(0,0,0)
    c.setStrokeAlpha(1)
    c.setFillAlpha(1)
    c.drawString(column1+1,row2-(iter*textgap),'{row_id_year1} {cause_medium_year1}'.format(cause_medium_year1=df.cause_medium_year1.ix[i],row_id_year1=df.row_id_year1.ix[i]))
    c.drawString(column2+1,row2-(iter*textgap),'{row_id_year2} {cause_medium_year2}'.format(cause_medium_year2=df.cause_medium_year2.ix[i],row_id_year2=df.row_id_year2.ix[i]))

    # set color for start year resid arrows
    c.setFillColorRGB(fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][0],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][1],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][2])
    c.setFillAlpha(0.2)
    c.setStrokeColorRGB(fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][0],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][1],fill['{cause_year1}'.format(cause_year1 = df.cause_year1.ix[i])][2])
    c.setStrokeAlpha(0.7)
    
    #determine rank change
    num_start = df[df.cause_medium_year1 == df.cause_medium_year1.ix[i]].index.values[0]
    num_end = df[df.cause_medium_year2 == df.cause_medium_year1.ix[i]].index.values[0]
    
    # detemine line type
    if num_start >= num_end:
        c.setDash(1,0)
    else:
        c.setDash(3,1)

    #draw line
    c.line(column1+columnwidth1,row2-((num_start+1)*textgap)+(.33*textsize),column2,row2-((num_end+1)*textgap)+(.33*textsize))

    # iterate
    iter = iter + 1
    


#### draw legend
c.setStrokeColorRGB(0,0,0)
c.setFillColorRGB(0,0,0)
c.setStrokeAlpha(1)
c.setFillAlpha(1)
c.setFont("Helvetica-Bold",textsize)
c.drawString(column3,row1-((legend_iter-10)*textgap),'Legend:')
c.setFont("Helvetica",textsize)
c.setStrokeAlpha(0.7)
c.setFillColorRGB(.7490196,.2470588,.2470588)
c.drawString(column3,row1-((legend_iter-10)*textgap)-textgap,'Communicable, maternal,')
c.drawString(column3,row1-((legend_iter-10)*textgap)-(1.75*textgap),'neonatal and nutritional')
c.setFillColorRGB(.2470588,.2470588,.7490196)
c.drawString(column3,row1-((legend_iter-10)*textgap)-(2.75*textgap),'Non-communicable')
c.setFillColorRGB(.2470588,.7490196,.2470588)
c.drawString(column3,row1-((legend_iter-10)*textgap)-(3.75*textgap),'Injuries')


#### save
c.save()
