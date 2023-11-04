#!/usr/bin/env python
#-*- coding:utf-8 -*-
""" 计算LM324电量指示电路的实际算法
    Author: cdhigh
"""

#标准阻值序列，200k以下
E12_RESISTORS = [
    #1.0,1.2,1.5,1.8,2.2,2.7,3.3,3.9,4.7,5.6,6.8,8.2,9.1,
    #10,12,15,18,22,27,33,39,47,56,68,82,91,
    100,120,150,180,220,270,330,390,470,560,680,820,910,
    1000,1200,1500,1800,2200,2700,3300,3900,4700,5600,6800,8200,9100,
    10000,12000,15000,18000,22000,27000,33000,39000,47000,56000,68000,82000,91000,
    100000,120000,150000,180000,]

#标准阻值序列，200k以下
E24_RESISTORS = [
    #1.0,1.1,1.2,1.3,1.5,1.6,1.8,2.0,2.2,2.4,2.7,3.0,3.3,3.6,3.9,4.3,4.7,5.1,5.6,6.2,6.8,7.5,8.2,9.1,
    #10,11,12,13,15,16,18,20,22,24,27,30,33,36,39,43,47,51,56,62,68,75,82,91,
    100,110,120,130,150,160,180,200,220,240,270,300,330,360,390,430,470,510,560,620,680,750,820,910,
    1000,1100,1200,1300,1500,1600,1800,2000,2200,2400,2700,3000,3300,3600,3900,4300,4700,5100,5600,6200,6800,7500,8200,9100,
    10000,11000,12000,13000,15000,16000,18000,20000,22000,24000,27000,30000,33000,36000,39000,43000,47000,51000,56000,62000,68000,75000,82000,91000,
    100000,110000,120000,130000,150000,160000,180000,]

E12_RATIOS = {} #键为比率，值为电阻对列表
E24_RATIOS = {}
E12_RATIOS_3R = {} #三个电阻比率，键为比率(两个比率拼接)，值为电阻元祖列表
E24_RATIOS_3R = {}

#计算标准电阻对的比率，生成 E12_RATIOS/E24_RATIOS
def calStdRatios():
    global E12_RATIOS, E24_RATIOS
    E12_RATIOS.clear()
    E24_RATIOS.clear()
    for resistor1 in E12_RESISTORS:
        for resistor2 in E12_RESISTORS:
            ratio = float(resistor1 / resistor2)
            E12_RATIOS.setdefault(ratio, []).append((resistor1, resistor2))
    for resistor1 in E24_RESISTORS:
        for resistor2 in E24_RESISTORS:
            ratio = float(resistor1 / resistor2)
            E24_RATIOS.setdefault(ratio, []).append((resistor1, resistor2))

calStdRatios()

#计算标准电阻对的比率(三个电阻)，需要的时候再计算
#返回比率字典
def calStdRatios3R(e24: int):
    global E12_RATIOS_3R, E24_RATIOS_3R
    ratios3R = {}
    stdResistors = E24_RESISTORS if e24 else E12_RESISTORS
    for resistor1 in stdResistors:
        for resistor2 in stdResistors:
            ratio1 = resistor1 / resistor2
            for resistor3 in stdResistors:
                ratio2 = resistor2 / resistor3
                #字典的键为一个tuple(ratio1, ratio2)
                ratios3R.setdefault((ratio1, ratio2), []).append((resistor1, resistor2, resistor3))
    if e24:
        E24_RATIOS_3R = ratios3R
    else:
        E12_RATIOS_3R = ratios3R
    return ratios3R

#在一个有序列表中找和给定值最接近的一个值
#返回对应的值
def closestValue(myDict, value):
    closestDiff = float('inf')
    closestTuple = None
    if isinstance(value, tuple):
        ratio1, ratio2 = value
        for (keyRatio1, keyRatio2), tup in myDict.items():
            diff = abs(keyRatio1 - ratio1) + abs(keyRatio2 - ratio2)
            if diff < closestDiff:
                closestDiff = diff
                closestTuple = tup
    elif isinstance(myDict, (list, tuple)):
        for elem in myDict:
            diff = abs(elem - value)
            if diff < closestDiff:
                closestDiff = diff
                closestTuple = elem
    else:
        for keyRatio, tup in myDict.items():
            diff = abs(keyRatio - value)
            if diff < closestDiff:
                closestDiff = diff
                closestTuple = tup

    return closestTuple

#在一个有序列表中找和给定值最接近的两个值，第一元素为最接近的，第二个元素为第二接近
#假定列表中元素个数大于2
#value需要和myDict里面的键类型一致，如果是float就需要传入float，如果是tuple则需要传入tuple
#返回: (value, value2)
def closest2Value(myDict, value):
    closestDiffs = [float('inf'), float('inf')]
    closestTuples = [None, None]
    if isinstance(value, tuple):
        ratio1, ratio2 = value
        for (keyRatio1, keyRatio2), tup in myDict.items():
            diff = abs(keyRatio1 - ratio1) + abs(keyRatio2 - ratio2)
            if diff < closestDiffs[0]:
                closestDiffs[1] = closestDiffs[0] #前面的一个元素移动到后面
                closestTuples[1] = closestTuples[0]
                closestDiffs[0] = diff
                closestTuples[0] = tup
            elif diff < closestDiffs[1]:
                closestDiffs[1] = diff
                closestTuples[1] = tup
    elif isinstance(myDict, (list, tuple)):
        for elem in myDict:
            diff = abs(elem - value)
            if diff < closestDiffs[0]:
                closestDiffs[1] = closestDiffs[0] #前面的一个元素移动到后面
                closestTuples[1] = closestTuples[0]
                closestDiffs[0] = diff
                closestTuples[0] = elem
            elif diff < closestDiffs[1]:
                closestDiffs[1] = diff
                closestTuples[1] = elem
    else:
        for keyRatio, tup in myDict.items():
            diff = abs(keyRatio - value)
            if diff < closestDiffs[0]:
                closestDiffs[1] = closestDiffs[0] #前面的一个元素移动到后面
                closestTuples[1] = closestTuples[0]
                closestDiffs[0] = diff
                closestTuples[0] = tup
            elif diff < closestDiffs[1]:
                closestDiffs[1] = diff
                closestTuples[1] = tup

    return closestTuples

#输入一个分压门限值(对应到四级门限的最低一级)，输出一对电阻取值，同时给出对应的修正门限
#e24: 是否使用E24电阻系列
#返回：[(upResistor, downResistor),]
def getResistorPairList(thresVol, refVol, e24):
    #上下电阻分压比
    expRatio = float((thresVol - refVol) / refVol)

    stdRatios = E24_RATIOS if e24 else E12_RATIOS

    #选择一对最接近的分压比电阻
    lst1, lst2 = closest2Value(stdRatios, expRatio)
    
    return (lst1[::-1] if lst1 else []) + (lst2[::-1] if lst2 else [])

#计算R3
#thresVol2: 第二级门限
#R2: R2值
#rDown: 下部电阻总和 R3+R4+R5+R6，就是getResistorPair()返回的列表里面的元祖第二个元素
def calR3(thresVol2, R2, rDown, refVol=2.5):
    TR = thresVol2 - refVol
    return ((rDown * TR) - (refVol * R2)) / (refVol + TR)

#计算R4
#thresVol3: 第三级门限
#R2, R3: R2/R3值
#rDown: 下部电阻总和 R3+R4+R5+R6，就是getResistorPair()返回的列表里面的元祖第二个元素
def calR4(thresVol3, R2, R3, rDown, refVol=2.5):
    TR = thresVol3 - refVol
    R23 = R2 + R3
    RD3 = rDown - R3
    return (RD3 - ((refVol / TR) * R23)) / ((refVol / TR) + 1)

#计算R5
#thresVol4: 第四级门限
#R2, R3, R4: R2/R3/R4值
#rDown: 下部电阻总和 R3+R4+R5+R6，就是getResistorPair()返回的列表里面的元祖第二个元素
def calR5(thresVol4, R2, R3, R4, rDown, refVol=2.5):
    TR = thresVol4 - refVol
    R234 = R2 + R3 + R4
    RD34 = rDown - R3 - R4
    return (RD34 - ((refVol / TR) * R234)) / ((refVol / TR) + 1)

#反向计算各分压门限值，用于验算
def calThresholds(R2, R3, R4, R5, R6, refVol=2.5):
    t1 = (R2 * refVol) / (R3 + R4 + R5 + R6) + refVol
    t2 = ((R2 + R3) * refVol) / (R4 + R5 + R6) + refVol
    t3 = ((R2 + R3 + R4) * refVol) / (R5 + R6) + refVol
    t4 = ((R2 + R3 + R4 + R5) * refVol) / R6 + refVol
    return t1, t2, t3, t4

#智能计算各个电阻值(电路图1)
#e24: 是否使用E24电阻系列，E24系列的电阻值选择多一些
#t1, t2, t3, t4为四级门限（从低到高）
#返回一个元祖：(R2, R3, R4, R5, R6)
def calAllResistors(t1, t2, t3, t4, refVol=2.5, e24=1):
    pairList = getResistorPairList(t1, refVol=refVol, e24=e24)
    if not pairList:
        return None

    ret = []
    stdResistors = E24_RESISTORS if e24 else E12_RESISTORS
    for R2, rDown in pairList:
        R3 = closestValue(stdResistors, calR3(t2, R2=R2, rDown=rDown, refVol=refVol))
        R4 = closestValue(stdResistors, calR4(t3, R2=R2, R3=R3, rDown=rDown, refVol=refVol))
        R5 = closestValue(stdResistors, calR5(t4, R2=R2, R3=R3, R4=R4, rDown=rDown, refVol=refVol))
        R6 = rDown - R3 - R4 - R5
        if R6 > 0.0:
            ret.append((R2, R3, R4, R5, R6))

    return ret

#==========================================================
#针对电路图2的算法

#智能计算各个电阻值(电路图2)
#e24: 是否使用E24电阻系列，E24系列的电阻值选择多一些
#t1, t2, t3, t4为四级门限（从低到高）
#返回一个元祖列表：[(R2, R3, R4, R5, R7, R8),]
def calAllResistorsSch2(t1, t2, t3, t4, refVol=2.5, e24=1):
    #先算R7/R8
    pairList = getResistorPairList(t4, refVol, e24)
    if not pairList:
        return None
    
    ret = []
    stdRatios = E24_RATIOS_3R if e24 else E12_RATIOS_3R
    if not stdRatios:
        stdRatios = calStdRatios3R(e24)

    for R7, R8 in pairList:
        #计算其他门限电压经R7/R8分压后的电压
        r78ratio = R8 / (R7 + R8)
        v2 = t3 * r78ratio #对应R2
        v3 = t2 * r78ratio
        v4 = t1 * r78ratio
        
        #电压比率，也是电阻比率
        ratio1 = (refVol - v2) / (v2 - v3)
        ratio2 = (v2 - v3) / (v3 - v4)
        
        #选择一组最接近的分压比电阻
        #ratio1 = round(ratio1 * 10000 * 100000000000000)
        #ratio2 = round(ratio2 * 1000000)
        expRatio = (ratio1, ratio2)
        lst1, lst2 = closest2Value(stdRatios, expRatio)
        if not lst1:
            continue
        
        for R2, R3, R4 in lst1[:-1]:
            #计算R5+R6，其上的电压为v4
            R5 = v4 * R4 / (v3 - v4)
            if R5 > 0.0:
                ret.append((R2, R3, R4, R5, R7, R8))
        for R2, R3, R4 in lst2[:-1]:
            #计算R5+R6，其上的电压为v4
            R5 = v4 * R4 / (v3 - v4)
            if R5 > 0.0:
                ret.append((R2, R3, R4, R5, R7, R8))

    return ret
    

#反向计算各分压门限值，用于验算（电路图2）
def calThresholdsSch2(R2, R3, R4, R5, R7, R8, refVol=2.5):
    t4 = refVol * (R7 + R8) / R8
    t3 = (refVol * (R3 + R4 + R5) * (R7 + R8)) / ((R2 + R3 + R4 + R5) * R8)
    t2 = (refVol * (R4 + R5) * (R7 + R8)) / ((R2 + R3 + R4 + R5) * R8)
    t1 = (refVol * R5 * (R7 + R8)) / ((R2 + R3 + R4 + R5) * R8)
    return t1, t2, t3, t4