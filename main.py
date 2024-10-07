from constants import *

def limit(p1, p2, x):
    return p1 if x <= p1 else x if x < p2 else p2

def insw(expr0, expr1, expr2):
    return expr1 if expr0 < 0 else expr2

def equation_of_motion():

    # wheel rotation equations
    # nose wheel
    thendd = (fnslip * fnt * rn) / int

    # left main wheel.
    thmldd = (fmslpl * fmtl * rml - brtorl) / imt

    # right main wheel
    if iasym == 1:
        thmrdd = (fmslpr * fmtr * rmr - brtorr) / imt

    # unsprung mass equations
    # nose gear
    zbnadd = ((fnt * costhe + fdnt * sinthe - fstal * cosgam + fstnl * singam) / mn
              - g * costhe)
    xbnadd = ((fdnt * costhe - fnt * sinthe - fstnl * cosgam - fstal * singam) / mn
              + g * sinthe)

    # left main gear
    zbmldd = (-fmgl + fdmtl * sinthe + fmtl * costhe) / mm - g * costhe
    xbmldd = (-fnmgl + fdmtl * costhe - fmtl * sinthe) / mm + g * sinthe

    # right main gear
    if iasym == 1:
        zbmrdd = (-fmgr + fdmtr * sinthe + fmtr * costhe) / mm - g * costhe
        xbmrdd = (-fnmgr + fdmtr * costhe - fmtr * sinthe) / mm + g * sinthe

    # Уравнения для жесткого тела.
    if irigid == 1:
        for i in range(ntm):
            qdd[i] = 0.0
        if time != 0.0:
            # Переход к блоку '254'
            pass
    else:
        # modal equations
        for i in range(nsm):
            j = 5 * i
            qdd[i] = (
                    (dlf[0] * ce[j] + dlf[1] * ce[j + 1] + (dlf[2] + dlf[4]) * ce[j + 2] +
                     (dlf[3] + dlf[5]) * ce[j + 3] + dlf[6] * ce[j + 4]) / (two * gm[i])
                    - gomeg[i] * qdot[i] - omsq[i] * q[i]
            )

        # Если iasym == 1, расчет дополнительных уравнений.
        if iasym == 1:
            for i in range(nam):
                k = nsm + i
                j = 5 * (k - 1)
                qdd[k] = (
                        ((dlf[2] - dlf[4]) * ce[j + 2]) / (two * gm[k])
                        - gomeg[k] * qdot[k] - omsq[k] * q[k]
                )

    for i in range(ntm):
        qddt[0][i] = qdd[i]

    # rigid vertical translation
    thrust = (18800.0 + 7.0532 * vfps) * th
    if kvtaxi == 1:
        thrust = weight * rolfri + drag

    # block '254'
    deldd = (
            ((fmgl + fmgr + fngv) * costhe - (fnmgl + fnmgr + fngl - thrust) * sinthe + lift)
            / mass - g
    )

    # rigid fore and afr translation
    vipsd = -(
            (fnmgl + fnmgr + fngl) * costhe + (fmgl + fmgr + fngv) * sinthe + drag - thrust * costhe
    ) / mass

    dawd = thed + (deld * vipsd / vips ** 2 - deldd / vips) / (one + (deld / vips) ** 2)
    aerom = -lift * dar + qs * cbar * (cm + cmh * delh + 0.5 * cbar * cmq * dawd / vips)

    # rigid pitch rotation
    thedd = (
            (fngv * dn - (fmgr + fmgl) * dm - fngl * lns - fnmgl * lmsl - fnmgr * lmsr
             + thrust * dth + aerom) / ithe
    )

    # rigid roll rotation
    if iasym == 1:
        phidd = (fmgl - fmgr) * arml / iphi
    else:
        phidd = 0.0
        phid = 0.0
        phi = 0.0

    zz0147 = zbnad
    zz0148 = xbnad
    zz0149 = zbmald
    zz0150 = xbmald
    zz0151 = zbmard
    zz0152 = xbmard

    for i in range(20):
        qddt[1][i] = qdott[0][i]

    zz0173 = deld
    zz0174 = vips
    zz0175 = thed
    zz0176 = phid
