shapiro.test(tfm$Edad[tfm$Tratamiento== "0"])
shapiro.test(tfm$Edad[tfm$Tratamiento== "1"])
shapiro.test(tfm$Edad[tfm$Tratamiento== "2"])

shapiro.test(tfm$IMC[tfm$Tratamiento== "0"])
shapiro.test(tfm$IMC[tfm$Tratamiento== "1"])
shapiro.test(tfm$IMC[tfm$Tratamiento== "2"])

kruskal.test(EsguinceDer~Tratamiento== "manual", data=tfm)
kruskal.test(EsguinceDer~Tratamiento== "ejercicio", data=tfm)
kruskal.test(EsguinceDer~Tratamiento== "control", data=tfm)

shapiro.test(tfm$EsguinceIzq[tfm$Tratamiento== "manual"])
shapiro.test(tfm$EsguinceIzq[tfm$Tratamiento== "ejercicio"])
shapiro.test(tfm$EsguinceIzq[tfm$Tratamiento== "control"])


aov(Edad~Tratamiento=="manual", data=tfm)
summary(aov(Edad~Tratamiento=="manual", data=tfm))
aov(Edad~Tratamiento=="ejercicio", data=tfm)
summary(aov(Edad~Tratamiento=="ejercicio", data=tfm))
aov(Edad~Tratamiento=="control", data=tfm)
summary(aov(Edad~Tratamiento=="control", data=tfm))

aov(IMC~Tratamiento=="manual", data=tfm)
summary(aov(IMC~Tratamiento=="manual", data=tfm))
aov(IMC~Tratamiento=="ejercicio", data=tfm)
summary(aov(IMC~Tratamiento=="ejercicio", data=tfm))
aov(IMC~Tratamiento=="control", data=tfm)
summary(aov(IMC~Tratamiento=="control", data=tfm))

aov(EsguinceIzq~Tratamiento=="manual", data=tfm)
summary(aov(EsguinceIzq~Tratamiento=="manual", data=tfm))
aov(EsguinceIzq~Tratamiento=="ejercicio", data=tfm)
summary(aov(EsguinceIzq~Tratamiento=="ejercicio", data=tfm))
aov(EsguinceIzq~Tratamiento=="control", data=tfm)
summary(aov(EsguinceIzq~Tratamiento=="control", data=tfm))
