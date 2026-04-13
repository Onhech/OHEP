
## 1. Brand Foundation

### Purpose

**We power transformational leadership growth.**

### Core Belief System

- Leaders are **developed, not born**
    
- Leadership growth should be:
    
    - **Early**
        
    - **Continuous**
        
    - **Embedded in real work**
        

### Strategic Positioning

- Focus: **Leadership development at scale**
    
- Audience: **Emerging + mid-level leaders**
    
- Differentiator: **Applied, continuous system vs. episodic training**
    

---

## 2. Brand Philosophy

### Key Assertions

- Leadership ≠ workshops or theory
    
- Growth requires:
    
    - Repetition
        
    - Context
        
    - Reinforcement
        

### Brand Metaphor

- **Monarch butterfly**
    
    - Transformation through stages
        
    - Structured evolution → not random growth
        

---

## 3. Brand Personality

### Traits

- Transformational
    
- Practical
    
- Systematic
    
- Credible
    
- Empowering
    

### Tone Rules

|Do|Avoid|
|---|---|
|Direct, outcome-driven|Abstract theory|
|Clear, structured|Buzzword-heavy language|
|Pragmatic|Inspirational fluff|

---

## 4. Brand Governance

### Core Principle

> Every interaction is a **brand signal**

### Non-Negotiables

- Consistency > creativity
    
- Clarity > cleverness
    
- Function > decoration
    

---

## 5. Visual Identity System

### Logo Rules

- Clear space = **1× logo height minimum**
    
- Minimum digital size: **24px height**
    
- Always:
    
    - Use approved colors
        
    - Ensure contrast
        

### Never

- Stretch
    
- Recolor arbitrarily
    
- Add shadows/effects
    
- Place on noisy backgrounds
    

---

## 6. Color System (Fully Specified)

### Core Palette (Monarch-Inspired)

|Role|Name|HEX|RGB|Usage|
|---|---|---|---|---|
|Primary|Monarch Orange|`#F05A28`|240, 90, 40|Primary CTA, highlights|
|Primary Dark|Deep Ember|`#C4451C`|196, 69, 28|Hover states, emphasis|
|Secondary|Charcoal|`#1F1F1F`|31, 31, 31|Headings, primary text|
|Secondary Light|Slate|`#5A5A5A`|90, 90, 90|Secondary text|
|Accent|Warm Sand|`#F4EDE4`|244, 237, 228|Backgrounds|
|Accent Cool|Soft Gray|`#E8E8E8`|232, 232, 232|UI surfaces|
|White|Pure White|`#FFFFFF`|255,255,255|Base|

---

### Color Usage Ratios

- **60%** Neutral (white / sand)
    
- **30%** Dark (charcoal / slate)
    
- **10%** Accent (orange)
    

👉 Orange should feel **intentional, not dominant**

---

### Accessibility Rules

- Body text must be:
    
    - `#1F1F1F` on light backgrounds
        
- Minimum contrast:
    
    - WCAG AA (4.5:1)
        
- Never:
    
    - Orange text on white for body copy
        

---

### CSS Tokens (Drop-in Ready)

```css
:root {
  --monark-primary: #F05A28;
  --monark-primary-dark: #C4451C;

  --monark-text-primary: #1F1F1F;
  --monark-text-secondary: #5A5A5A;

  --monark-bg-primary: #FFFFFF;
  --monark-bg-alt: #F4EDE4;
  --monark-bg-muted: #E8E8E8;
}
```

---

### Example Usage

```html
<button style="
  background-color: var(--monark-primary);
  color: white;
  padding: 12px 20px;
  border-radius: 6px;
  font-weight: 600;
">
  Primary CTA
</button>
```

---

## 7. Typography

### Font Stack

- Headline: **P22 Mackinac**
    
- Body: **Poppins**
    

---

### Web Fallback Stack

```css
font-family: "Poppins", -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
```

---

### Type Scale

|Role|Size|Weight|
|---|---|---|
|H1|48px|600–700|
|H2|36px|600|
|H3|28px|500|
|Body Large|18px|400|
|Body|16px|400|
|Caption|14px|400|

---

### Rules

- Headlines:
    
    - Slightly tighter tracking
        
    - Strong hierarchy
        
- Body:
    
    - Line height: **1.5–1.7**
        
- Avoid:
    
    - Overly dense text blocks
        
    - Center-aligning long text
        

---

## 8. Layout & Spacing System

### Grid

- Base: **8px system**
    

### Spacing Scale

|Token|Value|
|---|---|
|XS|4px|
|SM|8px|
|MD|16px|
|LG|24px|
|XL|32px|
|XXL|48px|

---

### Layout Principles

- Max width: **1100–1200px**
    
- Use:
    
    - Generous whitespace
        
    - Clear sections
        
- Avoid:
    
    - Dense clustering
        
    - Visual clutter
        

---

## 9. Visual Language

### Style Direction

- Clean
    
- Structured
    
- Intentional
    

---

### Icons

- Thin to medium stroke
    
- Minimal detail
    
- Consistent weight
    

---

### Imagery

**Do:**

- Real workplace environments
    
- Leadership in action
    
- Diverse but professional
    

**Don’t:**

- Stock clichés
    
- Overly staged scenes
    
- Abstract corporate visuals
    

---

### Graphic Motifs

- Subtle use of:
    
    - Lines
        
    - Grids
        
    - Flow structures
        

👉 Reflects **systems thinking**

---

## 10. Component Patterns

### Cards

```html
<div style="
  background: white;
  border-radius: 10px;
  padding: 24px;
  box-shadow: 0 2px 8px rgba(0,0,0,0.05);
">
  <h3>Card Title</h3>
  <p>Supporting content</p>
</div>
```

---

### Sections

- Alternate:
    
    - White
        
    - Sand background
        

---

### Buttons

|Type|Style|
|---|---|
|Primary|Orange fill|
|Secondary|Outline|
|Tertiary|Text only|

---

## 11. Key Takeaways (Consultant Lens)

### This Brand Is

- A **system**
    
- A **process**
    
- A **capability builder**
    

### This Brand Is NOT

- Motivational content
    
- Academic theory
    
- One-time intervention
    

---

### Strategic Execution Rule

> If it cannot be **applied immediately**, it’s off-brand.

---

## 12. Asset Creation Checklist

### Messaging

- Concrete?
    
- Actionable?
    
- System-oriented?
    

### Design

- Uses spacing system?
    
- Correct color ratios?
    
- Clean hierarchy?
    

### Typography

- Consistent scale?
    
- Readable?
    

### Final Test

- Could this be mistaken for:
    
    - A consulting firm?
        
    - A SaaS product?
        

👉 If yes → refine toward **leadership system clarity**

---

## 13. Quick Start (For Consultants)

To create anything fast:

1. Use:
    
    - White background
        
    - Charcoal text
        
    - Orange only for emphasis
        
2. Apply:
    
    - Large bold headline
        
    - Simple structure
        
3. Add:
    
    - One idea per section
        
4. Remove:
    
    - Anything decorative without function
        
